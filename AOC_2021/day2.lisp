(defparameter *input2* (uiop:read-file-lines "~/Documents/Lisp/AOC/AOC_2021/day2_input"))

(setf *input2* (map 'list (lambda (command) (apply #'concatenate 'string '("(" command ")")))))

(elt *input2* 0)

(defparameter *horizontal-position* 0)
(defparameter *depth* 0)

(defun up (x) (incf *depth* (* -1 x)))

(defun down (x) (incf *depth* x))

