(ql:quickload "uiop")
(ql:quickload "split-sequence")

(defparameter *testing* nil)
(defparameter *input* (list))

(let ((filename "~/Documents/Lisp/AOC/src/AOC_2022/inputs/6"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input* (coerce (uiop:read-file-line filename) 'list)))

(defun find-unique-subseq (num-char)
  (loop for i upfrom 0 below (- (length *input*) num-char)
        when (= num-char (length (remove-duplicates (subseq *input* i (+ i num-char)))))
        return (+ i num-char)))

(defun part-a () (find-unique-subseq 4))

(defun part-b () (find-unique-subseq 14))

(let ((a-ans (part-a))
      (b-ans (part-b)))
  (if *testing*
      (assert (= 10 a-ans))
      (print a-ans))
  (if *testing*
      (assert (= 29 b-ans))
      (print b-ans)))

