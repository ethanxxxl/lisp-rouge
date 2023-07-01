;;;; PROGRAMMER: Ethan Smith
;;;;
;;;; DATE: 21 June 2023
;;;;
;;;; DESCRIPTION:
;;;; This package provides methods for generating random noise.

(defpackage :xl-noise
  (:use :cl))

(in-package :xl-noise)

(defun smoothstep (x)
  (cond ((< x 0) 0)
        ((> x 1) 1)
        (t (+ (* 6 (expt x 5))
              (* -15 (expt x 4))
              (* 10 (expt x 3))))))

(defun interpolate (a0 a1 w)
  (+ a0 (* (smoothstep w)
           (- a1 a0))))

(defun ^2 (n) (expt n 2))

(defstruct gradient-array
  (:weight 'float)
  (:data '(array float 2)))

(defstruct perlin
  ;; width of structure in pixels
  (:height '(integer 0))
  ;; height of structure in pixels
  (:width '(integer 0))
  (:gradients '(cons gradient-array))
  ;; every vector must be the same length
  (:data '(array vector))
  (:random-state 'random-state))

(defun generate-gradients (weight dimensions &optional (rand-state *random-state*))
  "Generates a gradient-array with the given weight and dimensions."
  (make-gradient-array
   :weight weight
   :data (map-array (lambda (_) (declare (ignore _)) (random 1.0 rand-state))
                    (make-array dimensions :element-type 'float))))

(defun perlin-crunch-numbers (perlin)
  "returns a perlin array with the data filled out"
  (flet ((crunch-nums-iter (gradients width height)
           (if (second gradients)
               (map-arrays #'+
                           (rasterize-gradient (first gradients) width height)
                           (crunch-numbers-iter (cdr gradients) width height))
               (rasterize-gradient (first gradients) width height))))

    (setf (perlin-data (copy-perlin perlin))
          (crunch-nums-iter (perlin-gradients perlin)
                            (perlin-width perlin)
                            (perlin-height perlin)))))

(defun perlin-add-gradient (perlin gradient)
  "adds gradient to the gradients field in perlin.")

;; TODO convert the logic from previous iteration of noise to this function
(defun compute-gradient (gradient-array dimensions)
  "returns an array of RGB values, dimensions large.

In most cases, the returned array will have a rank of 2, and dimensions will be
of the form (height width), but higher dimensionality is also supported"
  (let ((result (make-array dimensions :element-type '(vector float 3))))
    ;; find the pixel value of the perlin noise at every index
    (map-array (lambda (_ i) (pixel-perlin gradient-array
                                      dimensions
                                      (array-subscripts results i)))
               results
               t)))

(defun vec-dot (v1 v2)
  "takes the dot product of v1 and v2. Assumes both vectors begin at the origin"
  (reduce #'+ (map 'vector #'* v1 v2)))

(defun pixel-perlin (gradient dimensions pixel-location)
  "returns the value of the 'pixel' located at `PIXEL-LOCATION'. `PIXEL-LOCATION'
is an list of array subscripts. Its length must match the rank of gradient.
`DIMENSIONS' is a list contaiing the dimensions of the pixel map being
generated. The rank of dimensions must match the rank of gradient."
  ;; you want to return the interpolated value of the dot product between the vector from each gradient
  ;; at the "corner" of each "cell" and the gradient at those corners.

  ;; first you have to find the corners of the higher dimensional cell.
  ;; they are just every permutation of (floor Dn) and (1+ (floor Dn)) for each dimension.

  )

(defun get-cell-corners (coords)
  "returns a list of coords for the corners of the cell that enclose the
given coorinates."
  (declare (optimize (debug 3)))

  ;; the number of corners for a n-dimensional cube is 2^n.
  ;; for every dimension d there will be a d + 1
  ;; for dim 0, create a list with the floor and the floor +1
  ;; then, copy the list, and add dim1 and dim1 + 1 to every other dimension.
  (labels ((map-append (tree item)
             "appends item to the end of every sublist in list"
             (mapcar (lambda (l) (append l (list item))) tree))
           (d0 (f) (floor (first f)))
           (d1 (f) (1+ (floor (first f))))
           (recurse (coords result)
             "recursively constructs a list of all corners in the cell specified by coords"
             (if coords
                 (recurse (cdr coords)
                          (append (map-append result (d0 coords))
                                  (map-append result (d1 coords))))
                 result)))

    ;; call the recursive function with an initial condition
    (recurse (cdr coords) (list (list  (d0 coords)) (list (d1 coords))))))

(defun dot-grid-gradient (ix iy x y state &optional width-hint)
  "Calculates the dot product between the gradient and the distance vector."
  (declare ((integer 0) ix iy) (float x y))

  (let ((dx (- x ix))                   ; calclate distance vector
        (dy (- y iy))                   ; |
        (gradient (gradient-at ix iy state width-hint)))

    ;; calculate/return dot product between distance vector and gradient
    (+ (* dx (vec2-x gradient))
       (* dy (vec2-y gradient)))))

(defun perlin (x y state &optional width-hint)
  "determine noise at coordinates x, y"
  (declare (float x y))
  (let*
      ;; determine grid cell coordinates
      ((x0 (floor x))
       (x1 (1+ x0))
       (y0 (floor y))
       (y1 (1+ y0))

       ;; interpolation weights
       (sx (- x x0))
       (sy (- y y0)))

    ;; there are gradients at each of the four corners.
    ;; x0,y1 ┌──────┐ x1, y1
    ;;       │      │
    ;;       │      │
    ;; x0,y0 └──────┘ x1, y0
    ;; we are going to take the dot product of the gradient with the distance
    ;; vector formed by that corner and x,y
    ;;
    ;; we will then interpolate between (interpolation of top left product and
    ;; top right product) and (interpolation of bottom left product and bottom
    ;; right product)

    (+ 0.5
       (* 0.5
          (interpolate
           (interpolate (dot-grid-gradient x0 y0 x y state width-hint)
                        (dot-grid-gradient x1 y0 x y state width-hint) sx)
           (interpolate (dot-grid-gradient x0 y1 x y state width-hint)
                        (dot-grid-gradient x1 y1 x y state width-hint) sx)
           sy)))))

(defun perlin-map (height width step &optional (state *random-state*))
  (declare (fixnum height width) ((float 0.0 1.0) step)
           (optimize (speed 3)))

  ;; create an array large enough to hold all perlin values.
  (let ((val-map (make-array (list (round (/ height step))
                                   (round (/ width step))))))
    (map-array (lambda (_ i)
                 (declare (ignore _))
                 (let* ((subscripts (array-subscripts val-map i))
                        (x (float (* (first subscripts) step)))
                        (y (float (* (second subscripts) step))))
                   (perlin x y state width)))
               val-map t)))

;; TODO: layers should only specify frequency and intensity. the size of the
;; image should be determined automatically in a top level.
;; FIXME: this is not complete
(defun perlin-multi-map (layers &optional (state *random-state*))
  (if (eql (length layers) 1)
      (apply #'perlin-map (car layers) state)
      (add-arrays (apply #'perlin-map (car layers) state)
                  (perlin-multi-map (cdr layers) state))))

(defun 1d-to-2d (index width)
  "returns (x y) where x and y are the dimensions given by index"
  (list (mod index width) (floor (/ index width))))

(defun array-subscripts (a index)
  "returns a list of subscripts for the element in `A' at `INDEX'. It is
essentially the inverse of `ARRAY-ROW-MAJOR-INDEX'

Subscripts are returned in row-major order. For example, if `A' has rank 3, then
the output would be of the form (z y x), which is how `ARRAY-ROW-MAJOR-INDEX'
takes it's input"
  ;; Let's define the notation N(X) to describe a single dimension array of
  ;; length X. An n dimensional array will be notated as
  ;; N(X_1) * N(X_2) * ... * (X_n)
  ;;
  ;; Say that you have an array N(AB), that is, its size is defined by A*B.
  ;; mapping N(AB) to N(A) * N(B) is simply:
  ;;   x = k % B
  ;;   y = k / B
  ;;
  ;; Where x and y are the subscripts, and k is the index. This funcion
  ;; functionality is implemented in the 1d-to-2d function in this package.
  ;;
  ;; This function could then be used to map an array of any dimension. Say you
  ;; have an array N(ABCDE), and you want to map it to N(A)*N(B)*N(C)*N(D)*N(E).
  ;;
  ;; You can apply 1d-to-2d on N(ABCDE), such that
  ;;    x = k % BCDE
  ;;    y = k / BCDE
  ;;
  ;; This maps N(ABCDE) to N(A) * N(BCDE), where y indexes into N(A), and x into
  ;; N(BCDE). We can apply 1d-to20 on N(BCDE), using x as the index:
  ;;    x1 = x % CDE
  ;;    y1 = x / CDE
  ;;
  ;; Now we have the mapping N(ABCDE) to N(A) * N(B) * N(CDE). This process is
  ;; repeated until the mapping is complete.
  ;;
  ;; It is worth noting that this function returns subscripts in row-major order
  ;; (ex, for a 3d array: (z y x)). This is how other functions in lisp work
  ;; with array subscripts and how multidimensional arrays in C are indexed.

  (loop with s1 = index
        ;; we want to apply 1d-to-2d once for every dimension.
        for i from 1 to (array-rank a)

        collect (let*
                    ;; size of dimensions which havent been mapped yet
                    ((dims (reduce #'* (array-dimensions a) :start i))
                     ;; do the new mapping
                     (val (1d-to-2d s1 dims)))

                  ;; set the index for the next iteration
                  (setf s1 (first val))
                  ;; return the subscript for this dimension
                  (car (last val)))))

(defun map-array (f a &optional include-index)
  "Applies `F' to every element in `A' when rank of `A' is greater than 1. returns
an array of the same type as `A'.

if `INCLUDE-INDEX' is not nil, then `F' should take take the index as an
additional parameter"

  ;; `RESULTS' will have the same number of dimensions as `A', and is where the
  ;; output of `F' will go.
  (let ((results (make-array (array-dimensions a))))
    ;; call `F' on each element of `A' making changes in `RESULTS'
    (loop for i below (array-total-size a)
          do (setf (row-major-aref results i)
                   (apply f (append
                             (list (row-major-aref a i))
                             (when include-index (list i))))))
    results))

(defun map-arrays (f a1 a2)
  "applies f to a1 and a2. a1 and a2 must have the same dimensions"
  (let ((results (make-array (array-dimensions a1))))
    (loop for i below (array-total-size results)
          do (setf (row-major-aref results i)
                   (funcall f (row-major-aref a1 i) (row-major-aref a2 i))))
    results))

(defun byte-list (bytes num)
  "creates a list of bytes (little endian) from number. The list will be `BYTES'
long"
  (declare ((integer 0) bytes num))
  ;; TODO: determine if num will fit into bytes

  (loop for i below bytes
        collect (ldb (byte 8 (* 8 i)) num)))

(defun array-bmp (a path)
  "writes the contents of array `A' to the file specified at `PATH' as a bitmap.
`A' must be a 2D array containing either floats for monochrome images or lists
of floats, for RGB pixels. Values should range from 0.0 to 1.0, but the function
will not fail on negative values. It simply takes the absolute value of those
values.

If the file specified at `PATH' exists, it is superceded by the data in `A'"
  (declare ((array float 2) a))
  (declare (optimize (speed 3)))

  ;; first, define the bitmap header fields. The number of bytes for each header
  ;; is very specific, so each value in the header lists is representative of a
  ;; single byte.
  (let* ((file-header-size 14)
         (bmpcoreheader-size 12)
         (data-array-size (* 8 (array-total-size a)))
         ;; create the file header from the elements defined above
         (file-header (concatenate 'list
                                   '(#x42 #x4D) ; BMP file header magic number
                                   (byte-list 4 (+ file-header-size
                                                   bmpcoreheader-size
                                                   data-array-size))
                                   '(#x00 #x00 #x00 #x00) ; reserved bytes
                                   (byte-list 4 (+ file-header-size
                                                   bmpcoreheader-size))))
         (bmpcoreheader (concatenate 'list
                                     ;; The core header is the simplest BMP
                                     ;; header
                                     (byte-list 4 bmpcoreheader-size)
                                     ;; WIDTH
                                     (byte-list 2 (array-dimension a 1))
                                     ;; HEIGHT
                                     (byte-list 2 (array-dimension a 0))
                                     ;; PLANES (should always be 1)
                                     (byte-list 2 1)
                                     ;; BITCOUNT, we are using 3 8 bit channels.
                                     (byte-list 2 24))))

    ;; open the file stream. append header information, then the array data.
    ;; existing files are overwritten.
    (with-open-file (f path :direction :output
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (write-sequence file-header f)
      (write-sequence bmpcoreheader f)

      ;; append the array data to the file.
      (loop for i below (array-total-size a)
            ;; write all the data in row-major order. Rows must be word aligned,
            ;; and may need padding at the end of every row.
            do (write-sequence
                (let* ((val (row-major-aref a i))
                       (width (array-dimension a 1))
                       ;; calculate number of padding bytes.
                       (padding (- 4 (mod (* 3 width) 4))))

                  ;; append data with any padding bytes
                  (append
                   ;; map the list of RGB floats to an 8-bit integer
                   (mapcar (lambda (n) (round (abs (* 255 n))))
                           ;; because `A' can be either a float or list, ensure
                           ;; that it is a list of floats either way, before
                           ;; passing it to the mapping function
                           (if (typep val 'list)
                               val
                               (make-list 3 :initial-element val)))

                   ;; add padding at end of line
                   (when (and (equal (second (array-subscripts a i))
                                     (1- width))
                              (/= padding 0 4)) ; no padding is needed for
                                                ; either 0 or 4 bytes
                     (make-list padding :initial-element 0))))
                f))
      (close f))))

(defun color-test-array (s &optional vert)
  "returns an array containing a black and white gradient in the direction
specified by `VERT'. Ie, if `VERT' is t, then it will be a vertical gradient.
Otherwise, the gradient is horizontal. The size of the array is controlled by
`S'.

The array is composed of single float values ranging from 0.0 to 1.0"

  ;; create results array to act as an accumulator
  (let ((results (make-array (list s s))))
    ;; loop through the entire array in row-major order
    (loop for i below (array-total-size results)
          ;; w is the x or y axis, depending on `VERT'
          do (let* ((w (if vert
                           (first (array-subscripts results i))
                           (second (array-subscripts results i))))
                    (val (float (/ w s))))
               (setf (row-major-aref results i) val)))
    results))

(defun color-test-array2 (s)
  "Creates a checker pattern with 3 different colors. S controls the size of the square.
This function returns an array of pixels, where each pixel is a list of RGB
values."

  ;; create the results array, to be used as an accumulator
  (let ((results (make-array (list s s))))
    ;; loop through the entire array in row-major order.
    (loop for i below (array-total-size results)
          ;; keep track of x and y each iteration by getting the the subscripts
          ;; from the index
          do (let* ((y (first (array-subscripts results i)))
                    (x (second (array-subscripts results i)))
                    ;; val is the pixel value. iit depends on the position
                    ;; position of the pixel in the array.
                    (val (cond ((and (evenp (round x 10))
                                     (evenp (round y 10)))
                                (hex-to-rgb #xAB3229))
                               ((and (oddp (round x 10))
                                     (oddp (round y 10)))
                                (hex-to-rgb #x359CCC))
                               (t
                                (hex-to-rgb #x6B605F)))))
               ;; set the value in the array to the chosen val.
               (setf (row-major-aref results i) val)))
    results))

(defun hex-to-rgb (hex)
  "takes an RGB hex value of the form #xAABBCC. Returns a list of RGB floats
ranging from 0.0 to 1.0.

ex: (hex-to-rgb #xAABBCC) => (0.8 0.73333335 0.6666667)"
  (mapcar (lambda (h) (float (/ h 255))) (byte-list 3 hex)))
