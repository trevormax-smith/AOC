(ql:quickload "numcl")
(ql:quickload "array-operations")

(defpackage #:aoc_day22
  (:use :cl)
  (:shadowing-import-from #:numcl "ASARRAY" "MAKE-ARRAY" "AREF" "SUM"))
(in-package #:aoc_day22)

(defparameter *testing* nil)
(defparameter *input22* nil)
(defparameter *reactor-cores* (asarray (make-array '(101 101 101) :initial-element 0)))

(defun clean-input (i)
  (let ((sp (uiop:split-string (subseq i 2) :separator "..")))
    (list 
      (max (+ 50 (parse-integer (car sp))) 0)
      (max (+ 50 (+ 1 (parse-integer (caddr sp)))) 0))))

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day22_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (loop with my-array = '()
        for row in (uiop:read-file-lines filename) do
        (destructuring-bind (command xyz) (uiop:split-string row :separator " ")
          (setf command (if (string= "on" command) 1 0))
          (destructuring-bind (x y z) (map 'list 'clean-input (uiop:split-string xyz :separator ","))
            (push (list command x y z) my-array)))
        finally (setf *input22* (reverse my-array))))

(defun exec-input (input)
  (destructuring-bind (c x y z) input
    (if (find t (map 'list (lambda (v) (and (= 0 (car v)) (= 0 (cadr v)))) (list x y z)))
        nil
        (setf (aref *reactor-cores* x y z) c))))

(defun solve-a ()
  (mapc 'exec-input *input22*)
  (sum *reactor-cores*))

; (defun )

(let ((ans-a (solve-a)))
  (if *testing* (assert (= ans-a 590784)))
  (print ans-a))

#|
(let ((ans-b (numcl:amax (funcall my-board-state 0 0
                                  (if *testing* 4 8)
                                  (if *testing* 8 3)))))
  (if *testing* (assert (= ans-b 444356092776315)))
  (print ans-b))
|#
