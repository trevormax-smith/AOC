(ql:quickload "uiop")
(ql:quickload "split-sequence")

(defparameter *testing* t)
(defparameter *input* (list))

(defmacro split-splitter (delim)
  `(lambda (x) (mapcar (lambda (y) (split-sequence:split-sequence ,delim y)) x)))

(let ((filename "~/Documents/Lisp/AOC/src/AOC_2022/inputs/4"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input* (mapcar (lambda (x) (mapcar (lambda (y) (mapcar 'parse-integer y)) x))
                        (mapcar (split-splitter #\-)
                                 (funcall (split-splitter #\,)
                                          (uiop:read-file-lines filename))))))

(defun part-a () 2)

(defun part-b () 0)

(let ((a-ans (part-a))
      (b-ans (part-b)))
  (if *testing*
      (assert (= 2 a-ans))
      (print a-ans))
  (if *testing*
      (assert (= 0 b-ans))
      (print b-ans)))

