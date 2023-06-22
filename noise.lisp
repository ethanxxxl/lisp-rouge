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

(defun perlin-map (height width step &optional (state *random-state*))
  (loop for y below height by step
        collect (loop for x below width by step
                      collect (perlin (float x) (float y) state))))

;;; Bit Map Stuff
;;;
;;; Bitmap File Header
;;; DIB Header
;;; Pixel Array
(defun bmp-create-header (bmp-data))

(defun perlin-bmp (height width step)
  (with-open-file (f "./perlin.bmp" :direction :output
                                    :if-exists :supersede
                                    :element-type 'unsigned-byte)
    (write-sequence '(#\B #\M))
    (close f)))

;; test random gradient
(defun random-gradient-test ()
  (let ((g (random-gradient)))
    (sqrt (+ (^2 (vec2-x g))
             (^2 (vec2-y g))))))
