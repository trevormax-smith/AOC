(ql:quickload "numcl")
(ql:quickload "array-operations")

(defpackage #:aoc_day19
  (:use :cl)
  (:shadowing-import-from #:numcl "MAKE-ARRAY" "ASARRAY" "RESHAPE" "+" "UNSTACK" "STACK"))
(in-package #:aoc_day19)

(defparameter *testing* t)
(defparameter *input19* nil)

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day19_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input19* (uiop:read-file-lines filename))
  (setf *input19* 
    (let ((new-19 '(())))
      (dolist (item *input19*)
        (cond
          ((string= item "") (push '() new-19))
          ((find #\, item)
           (push (eval (read-from-string
                   (concatenate 'string "(asarray '(" (substitute #\  #\, item) "))")))
                 (elt new-19 0)))))
      new-19))
  (setf *input19* (map 'list (lambda (x) (asarray x)) *input19*)))

(setf *input19* (loop for inp in *input19*
                      for ind from 0 to (length *input19*) collect (list ind inp)))
(defparameter *input19-b* (loop for i in *input19* collect (make-array '(0 0 0))))

(defun vec-mat-mult (vec mat)
  (aops:each-index j
    (aops:sum-index i 
      (* (aref vec i) (aref mat j i)))))

(defun apply-rotation (scanner rotation)
  (asarray
    (let ((beacons (aops:split scanner 1)))
      (aops:each-index i
        (vec-mat-mult (aref beacons i) rotation)))))

(defun vdiff (b1 b2)
  (asarray (aops:each #'- b1 b2)))

(defun find-offsets (scanner-one scanner-two)
  (loop with split-one = (unstack (elt scanner-one 1))
        with split-two = (unstack (elt scanner-two 1))
        with scanner-one-dim = (array-dimension (elt scanner-one 1) 0)
        with scanner-two-dim = (array-dimension (elt scanner-two 1) 0)
        with total-length = (+ scanner-one-dim scanner-two-dim)
        for k from 0 below scanner-one-dim do
        (loop for j from 0 below scanner-two-dim do
              (let ((shifted-two (unstack (numcl:+ (elt scanner-two 1)
                                                   (vdiff (elt split-one k)
                                                          (elt split-two j))))))
                (let ((combined (union split-one shifted-two :test (lambda (x y) (= (elt x 0) (elt y 0))))))
                  (when (<= 12 (- total-length (length combined)))
                    (progn
                      (print (list scanner-one-dim scanner-two-dim))
                      (print `(,total-length ,(length combined)))
                      (return-from find-offsets (stack combined))))))))
  nil)

(defun cross-multiply (a b)
   (make-array 3
   :initial-contents (list (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1)))
                           (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2)))
                           (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0))))))

(defun construct-ivector (i s)
  (make-array 3 :initial-contents
              (case i
                (0 (list s 0 0))
                (1 (list 0 s 0))
                (2 (list 0 0 s)))))

(defun construct-rotation (xi yi xval yval)
  (let ((xvec (construct-ivector xi xval))
        (yvec (construct-ivector yi yval)))
    (aops:stack-rows xvec yvec (cross-multiply xvec yvec))))

(defparameter rotations
  (let ((rots '()))
    (loop for xi from 0 to 2 do
          (loop for xval in '(-1 1) do
                (loop for yi from 0 to 2
                      when (/= xi yi) do
                      (loop for yval in '(-1 1)
                            do (push (construct-rotation xi yi xval yval) rots)))))
    rots))

(defun check-dimensions (scanner-one scanner-two)
  (loop for rot in rotations
        ; do (print rot)
        do (let ((beacon-set (find-offsets scanner-one
                                          (list (elt scanner-two 0) (apply-rotation (elt scanner-two 1) rot)))))
             (when beacon-set (return (list (elt scanner-two 0) beacon-set))))))

(defun solve-a ()
  (loop for from-ind upfrom 1 to 255 do
        (loop for into-ind upfrom 2 to 127
              unless (= (mod from-ind (length *input19*)) (mod into-ind (length *input19*)))
              do (let ((maybe-merged (check-dimensions (elt *input19* (mod into-ind (length *input19*)))
                                                       (elt *input19* (mod from-ind (length *input19*))))))
                   (if maybe-merged
                       (progn
                         (print (length *input19*))
                         (setf (elt *input19* (mod into-ind (length *input19*))) maybe-merged)
                         (setf *input19* (remove (elt *input19* (mod from-ind (length *input19*)))
                                          *input19* :test (lambda (x y) (= (elt x 0) (elt y 0)))))
                         (print (length *input19*))
                         ; (print (apply '+ (map 'list 'array-dimension *input19*
                                               ; (loop repeat (length *input19*) collect 0))))
                                               )))
              when (= 1 (length *input19*)) do (return-from solve-a (array-dimension (elt (elt *input19* 0) 1) 0)))))

(let ((ans-a (solve-a)))
  (if *testing* (assert (= ans-a 79)))
  (print ans-a))

; (let ((ans-b (solve-b)))
  ; (if *testing* (assert (= ans-b 3993)))
  ; (print ans-b) *input19*)
