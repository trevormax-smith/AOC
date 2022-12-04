(defparameter *input14* (uiop:read-file-lines "~/Documents/Lisp/input14"))

(deftype mask-bit () '(member '0 '1 'X))

(defparameter *mask* (make-array 36 :element-type 'mask-bit :initial-element 'X))

(print *mask*)

(defparameter *memory* (make-hash-table))

(defun read-mask (val)
  (eval (read-from-string 
          (str:join ""
            (list "(setf *mask* (make-array " 
                  (str:join " " (map 'list 'string (coerce val 'list)))
                  ") :element)")))))
