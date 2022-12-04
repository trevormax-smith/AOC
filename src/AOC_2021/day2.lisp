(defparameter *input2* (uiop:read-file-lines "~/Documents/Lisp/AOC/AOC_2021/day2_input"))

(setf *input2* (map 'list (lambda (command) (apply #'concatenate 'string (list "(" command ")"))) *input2*))

(defparameter *horizontal-position* 0)
(defparameter *depth* 0)

(defun up (x) (incf *depth* (* -1 x)))
(defun down (x) (incf *depth* x))
(defun forward (x) (incf *horizontal-position* x))

(defun exec-input (x)
  (mapc (lambda (x) (eval (read-from-string x))) *input2*))

(exec-input *input2*)

(print (* *horizontal-position* *depth*))

(setf *horizontal-position* 0)
(setf *depth* 0)
(defparameter *aim* 0)

(defun forward (x)
  (list
    (incf *horizontal-position* x) 
    (incf *depth* (* *aim* x))))

(exec-input *input2*)

(print (* *horizontal-position* *depth*))
