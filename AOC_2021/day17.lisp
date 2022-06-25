(defparameter *testing* nil)
(defparameter *input17* nil)
(ql:quickload "array-operations")
(ql:quickload "select")
(ql:quickload "fset")

(if *testing*
    (setf *input17* '((20 30) (-10 -5)))
    (setf *input17* '((32 65) (-225 -177))))

(defparameter x-pos 0)
(defparameter y-pos 0)
(defparameter x-vel 0)
(defparameter y-vel 0)

(defun set-x-y-vel (x y)
  (setf x-vel x)
  (setf y-vel y))

(defun take-one-step ()
  (incf x-pos x-vel)
  (incf y-pos y-vel)
  (cond ((> x-vel 0) (decf x-vel))
        ((< x-vel 0) (incf x-vel)))
  (decf y-vel)
  (list x-pos y-pos))

#|
Dist = A
A = b h 1/2
b = h - 1
A = 1/2 (h^2 - h)
A = 1/2 (h^2 - h + 1/4) - 1/8
A = 1/2 (h - 1/2)^2 - 1/8
2A = (h - 1/2)^2
root(2A) = h - 1/2
root(2A) + 1/2 = h
|#

(defun solve-a ()
  (set-x-y-vel 
    (floor (expt
             (* (elt (elt *input17* 0) 1) 2)
             1/2))
    (* -1 (+ 1 (elt (elt *input17* 1) 0))))
  (values (/ (* y-vel (+ y-vel 1)) 2)))

(defun has-not-missed-p ()
  (and (>= y-pos (elt (elt *input17* 1) 0))
       (<= x-pos (elt (elt *input17* 0) 1))))

(defun is-inside-p ()
  (and
    (and
      (>= y-pos (elt (elt *input17* 1) 0)) 
      (<= y-pos (elt (elt *input17* 1) 1)))
    (and
      (>= x-pos (elt (elt *input17* 0) 0)) 
      (<= x-pos (elt (elt *input17* 0) 1)))))

(defun check-velocity (x-init-vel y-init-vel)
  (setf x-pos 0)
  (setf y-pos 0)
  (set-x-y-vel x-init-vel y-init-vel)
  (loop
    (when (or (is-inside-p)
              (not (has-not-missed-p)))
      (return (is-inside-p)))
    (take-one-step)))

(defun solve-b ()
  (let ((x-max (elt (elt *input17* 0) 1))
        (y-max y-vel)
        (y-min (elt (elt *input17* 1) 0)))
    (apply '+
           (loop for x upfrom 1 to x-max collect
                 (count t
                        (loop for y upfrom y-min to y-max collect
                              (check-velocity x y)))))))

(let ((ans-a (solve-a)))
  (if *testing* (assert (= ans-a 45)))
  (print ans-a))

(let ((ans-b (solve-b)))
  (if *testing* (assert (= ans-b 112)))
  (print ans-b))
