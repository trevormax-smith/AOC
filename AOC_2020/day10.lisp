(defparameter *input10* (uiop:read-file-lines "~/Documents/Lisp/input10"))

(setf *input10* (map 'list (lambda (x)
             (parse-integer (string x))) *input10*))

(setf *input10* (append '(0 (apply 'max *input10*) *input10*)))

(setf *input10* (sort *input10*))
