(ql:quickload "uiop")

(defparameter *testing-a* t)
(defparameter *testing-b* nil)
(defparameter *input* '())

(let ((filename "~/Documents/Lisp/AOC/src/AOC_2023/inputs/1"))
  (if *testing-a* (setf filename (concatenate 'string filename "_test_a")))
  (if *testing-b* (setf filename (concatenate 'string filename "_test_b")))
  (setf *input* (uiop:read-file-lines filename)))

(defun part-a () 8)

(defun part-b ())

(if *testing-a*
    (let ((a-ans (part-a))) 
      (assert (= 8 a-ans))
      (print a-ans))
    (print (part-a)))
(if *testing-b*
    (let ((b-ans (part-b)))
      (assert (= 281 b-ans))
      (print b-ans))
    (print (part-b)))
