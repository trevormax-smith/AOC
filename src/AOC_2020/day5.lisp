(defparameter *input5* (uiop:read-file-lines "~/Documents/Lisp/input5"))

(defun bit-vector-to-integer (bits)
  (reduce #'(lambda (a b) (+ (ash a 1) b)) bits))

(defmacro parse-pass (low high start stop)
  `(bit-vector-to-integer 
    (map 'bit-vector 
         'digit-char-p 
         (substitute #\1 ,high 
                     (substitute #\0 ,low 
                                 (subseq pass ,start ,stop))))) )

(defun parse-row (pass)
  (parse-pass #\F #\B 0 7))

(defun parse-col (pass)
  (parse-pass #\L #\R 7 10))

(defun parse-seat-ID (pass)
  (+ (* (parse-row pass) 8) (parse-col pass)))

(setf *input5* (map 'list 'parse-seat-id *input5*))
