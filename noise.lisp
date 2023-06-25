;;;; PROGRAMMER: Ethan Smith
;;;;
;;;; DATE: 21 June 2023
;;;;
;;;; DESCRIPTION:
;;;; This package provides methods for generating random noise.

(defpackage :xl-noise
  (:use :cl))

(in-package :xl-noise)

(defun interpolate (a0 a1 w)
  (+ (* (- a1 a0) w) a0))

(defstruct vec2 (:x 'integer) (:y 'integer))
(defun ^2 (n) (expt n 2))

(defun gradient-at (ix iy state)
  "returns the gradient at ix and iy. State is not modified."
  (declare ((integer 0) ix iy))

  ;; this algorithm creates copies of state for each axis. The random values in
  ;; each dimension are summed up, and added together. This is done to avoid
  ;; symetries occuring in cases where x + y are the same (or any other
  ;; mathematical operation between x and y).
  (let* ((x-state (make-random-state state))
         (y-state (make-random-state state))
         (r
           (+ (loop repeat ix
                    sum (random 1.0 x-state))
              (loop repeat iy
                    sum (random 1.0 y-state)))))

    (make-vec2 :x (cos r)
               :y (sin r))))

(defun dot-grid-gradient (ix iy x y state)
  "Calculates the dot product between the gradient and the distance vector."
  (declare ((integer 0) ix iy) (float x y))

  (let ((dx (- x ix))                 ; calclate distance vector
        (dy (- y iy))                 ; |
        (gradient (gradient-at ix iy state)))

    ;; calculate/return dot product between distance vector and gradient
    (+ (* dx (vec2-x gradient))
       (* dy (vec2-y gradient)))))

(defun perlin (x y state)
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
    ;; x0,y0 ┌──────┐ x1, y0
    ;;       │      │
    ;;       │      │
    ;; x0,y1 └──────┘ x1, y1
    ;; we are going to take the dot product of the gradient with the distance
    ;; vector formed by that corner and x,y
    ;;
    ;; we will then interpolate between (interpolation of top left product and
    ;; top right product) and (interpolation of bottom left product and bottom
    ;; right product)

    (interpolate (interpolate (dot-grid-gradient x0 y0 x y state)
                              (dot-grid-gradient x1 y0 x y state) sx)
                 (interpolate (dot-grid-gradient x0 y1 x y state)
                              (dot-grid-gradient x1 y1 x y state) sx)
                 sy)))

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

                  (setf s1 (first val)) ; set the index for the next iteration
                  (car (last val)))))   ; retun the subscript for this dimension

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
                   (funcall f (row-major-aref a i) (when include-index i))))
    results))

(defun perlin-map (height width step &optional (state *random-state*))
  (declare ((integer 0) height width) ((float 0.0 1.0) step))

  ;; create an array large enough to hold all perlin values.
  (let ((val-map (make-array (list (round (/ height step))
                                   (round (/ width step))))))
    (map-array (lambda (_ i)
                 (declare (ignore _))
                 (let* ((subscripts (array-subscripts val-map i))
                        (x (float (/ (first subscripts) width)))
                        (y (float (/ (second subscripts) height))))
                   (perlin x y state)))
               val-map t)))

(defun byte-list (bytes num)
  "creates a list of bytes (little endian) from number. The list will be `BYTES'
long"
  (declare ((integer 0) bytes num))
  ;; TODO: determine if num will fit into bytes

  (loop for i below bytes
        collect (ldb (byte 8 (* 8 i)) num)))

(defun array-bmp (a path)
  "writes the contents of array `A' to the file specified at `PATH' as a bitmap"
  (declare (optimize (debug 3)))
  (let* ((file-header-size 14)
         (bmpcoreheader-size 12)
         (data-array-size (* 8 (array-total-size a)))
         (file-header (concatenate 'list
                                   '(#x42 #x4D)
                                   (byte-list 4 (+ file-header-size
                                                   bmpcoreheader-size
                                                   data-array-size))
                                   '(#x00 #x00 #x00 #x00) ; reserved bytes
                                   (byte-list 4 (+ file-header-size
                                                   bmpcoreheader-size))))
         (bmpcoreheader (concatenate 'list
                                     (byte-list 4 bmpcoreheader-size)
                                     (byte-list 2 (array-dimension a 0))
                                     (byte-list 2 (array-dimension a 1))
                                     (byte-list 2 1)
                                     (byte-list 2 24))))
    (with-open-file (f path :direction :output
                                      :if-exists :supersede
                                      :element-type 'unsigned-byte)

      (write-sequence file-header f)
      (write-sequence bmpcoreheader f)
      (loop for i below (array-total-size a)
            do (write-sequence (let* ((val (row-major-aref a i))
                                      (uint (round (abs (* 255 val))))
                                      (width (array-dimension a 0))
                                      (padding (- 4 (mod (* 3 width) 4))))

                                 (append
                                  (list uint uint uint)

                                  ;; add padding at end of line
                                  (when (and (equal (second (array-subscripts a i))
                                                    (1- width))
                                             (/= padding 0))
                                    (make-list padding :initial-element 0))))
                               f))
      (close f))))

(defun color-test-array (s &optional vert)
  (let ((results (make-array (list s s))))
    (loop for i below (array-total-size results)
          do (let* ((w (if vert
                           (first (array-subscripts results i))
                           (second (array-subscripts results i))))
                    (val (float (/ w s))))
               (setf (row-major-aref results i) val)))
    results))
