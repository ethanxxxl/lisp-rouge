;;;; PROGRAMMER: Ethan Smith
;;;;
;;;; DATE: 21 June 2023
;;;;
;;;; DESCRIPTION:
;;;; This package provides methods for generating random noise.

(defpackage :xl-noise
  :use :cl)

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

;; this doesn't work because mutli-dimensional arrays aren't sequence types.
;;
;; TODO: create your own mapping function to map over a 2 dimensional array,
;; or otherwise find another way to do this.
(defun perlin-map (height width step &optional (state *random-state*))
  (let ((val-map (make-array (list (* height step) (* width step))))
        ;; indexes for map iteration
        (x 0.0)
        (y 0.0))

    (map '(array float (* *))
         (lambda (row)
           (prog1 (map 'float
                       (lambda (val)
                         (prog1 (perlin x y state)
                           (incf x step)))
                       row)
             (incf y step)
             (setf x 0.0)))
         val-map)))

;; apparently this doesn't work because multi-dimensional arrays aren't sequence
;; types
(format t "~&~S"
        (let ((a (make-array '(5 2) :element-type 'float :initial-element 1.0)))
          (map '(array float (5 2)) #'1+ a)))

;;; Bit Map Stuff
;;;
;;; Bitmap File Header
;;; DIB Header
;;; Pixel Array
(defun bmp-create-file-header (bmp-data))
(defun bmp-create-DIB-header ())
(defun bmp-create-pixel-array ())

(defun perlin-bmp (height width step)
  (with-open-file (f "./perlin.bmp" :direction :output
                                    :if-exists :supersede
                                    :element-type 'unsigned-byte)
    (write-sequence '(#\B #\M))))

;; test random gradient
(defun random-gradient-test ()
  (let ((g (random-gradient)))
    (sqrt (+ (^2 (vec2-x g))
             (^2 (vec2-y g))))))
