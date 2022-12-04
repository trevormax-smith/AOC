(ql:quickload "uiop")
(ql:quickload "split-sequence")

(defparameter *testing* nil)
(defparameter *input* (list))

(defmacro split-splitter (delim)
  `(lambda (x) (mapcar (lambda (y) (split-sequence:split-sequence ,delim y)) x)))

(let ((filename "~/Documents/Lisp/AOC/src/AOC_2022/inputs/4"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input* (mapcar (lambda (x) (mapcar (lambda (y) (mapcar 'parse-integer y)) x))
                        (mapcar (split-splitter #\-)
                                 (funcall (split-splitter #\,)
                                          (uiop:read-file-lines filename))))))

(defun fully-contains? (pair &optional (flag nil))
  (or (and (>= (car (car pair)) (car (cadr pair)))
           (<= (cadr (car pair)) (cadr (cadr pair))))
      (if flag nil (fully-contains? (reverse pair) t))))

(defun overlaps? (pair &optional (flag nil))
  (or (and (>= (cadr (cadr pair)) (car (car pair)))
           (<= (cadr (cadr pair)) (cadr (car pair))))
      (if flag nil (overlaps? (reverse pair) t))))

(defun part-a () (count 't (mapcar 'fully-contains? *input*)))

(defun part-b () (count 't (mapcar 'overlaps? *input*)))

(let ((a-ans (part-a))
      (b-ans (part-b)))
  (if *testing*
      (assert (= 2 a-ans))
      (print a-ans))
  (if *testing*
      (assert (= 4 b-ans))
      (print b-ans)))

