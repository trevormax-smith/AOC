(defparameter *input2* (uiop:read-file-lines "~/Documents/Lisp/input2"))

(defun first-int (pass)
  (parse-integer
    (subseq pass 0 (position #\- test))))

(defun second-int (pass)
  (parse-integer
    (subseq pass (+ 1 (position #\- test))
            (position #\  test))))

(parse-integer
  (subseq test (+ 1 (position #\- test))
          (position #\  test)))
