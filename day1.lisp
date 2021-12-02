(defparameter *input1* (uiop:read-file-lines "~/Documents/Lisp/AOC_2021/day1_input"))

(setf *input1* (map 'list 'parse-integer *input1*))

(setf *input1* (map 'list (lambda (x y) (> x y)) (subseq *input1* 1) (subseq *input1* 0 (length *input1*)))) 

(apply '+ *input1*)
