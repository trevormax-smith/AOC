(defparameter *input1* (uiop:read-file-lines "~/Documents/Lisp/AOC/AOC_2021/day1_input"))

(setf *input1* (map 'list 'parse-integer *input1*))

(defun zip-from-ends (seq number-of-slices n)
  (subseq seq n (- (length seq) (- number-of-slices n))))

(defun count-increases (seq)
  (count 'T (map 'list (lambda (x y) (< x y)) (zip-from-ends seq 1 0) (zip-from-ends seq 1 1))))

(print (count-increases *input1*))

(defparameter part-b (map 'list '+ (zip-from-ends *input1* 2 0) (zip-from-ends *input1* 2 1) (zip-from-ends *input1* 2 2)))

(count-increases part-b)
