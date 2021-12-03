(defparameter *input3* (uiop:read-file-lines "~/Documents/Lisp/AOC/AOC_2021/day3_input"))

#|
(setf *input3* '(
"00100"
"11110"
"10110"
"10111"
"10101"
"01111"
"00111"
"11100"
"10000"
"11001"
"00010"
"01010"))
|#

(defmacro get-nth-item (n) `(lambda (seq) (elt seq ,n)))

(defun count-ones (n seq)
  (count #\1 (map 'list (get-nth-item n) seq))  )

(defun get-one-occurances (seq)
  (loop for n from 0 to (- (length (elt seq 0)) 1) 
        collect (count-ones n seq)))

(defparameter one-occurances (get-one-occurances *input3*))

(defparameter gamma-bitvector
  (loop 
    for num-ones in one-occurances
    for pow-two downfrom (- (length one-occurances) 1) to 0
    when (< (length *input3*) (* 2 num-ones)) collect (expt 2 pow-two)))

(defparameter gamma-rate (reduce '+ gamma-bitvector))

(defparameter epsilon-rate (- (- (expt 2 (length one-occurances)) 1) gamma-rate))

(print (* gamma-rate epsilon-rate))

(defparameter inputcopy (copy-list *input3*))

(defun decide-value-oxygen (n-ones seq-length)
  (if (>= (* 2 n-ones) seq-length) #\1 #\0))

(defun decide-value-co2 (n-ones seq-length)
  (if (>= (* 2 n-ones) seq-length) #\0 #\1))

(defun reduce-sequency-via-decider (decider)
  (dotimes (n (+ (length one-occurances) 1)) 
    (if (= (length inputcopy) 1) (return inputcopy)
        (let ((choice-number (funcall decider (count-ones n inputcopy) (length inputcopy))))
          (print inputcopy)
          (print choice-number)
          (setf inputcopy (remove-if-not (lambda (num) (char= choice-number (elt num n))) inputcopy))))))

(defun bit-string-to-integer (bit-string)
  (reduce '+ 
          (loop for value in (coerce bit-string 'list) 
                for pow-two downfrom (- (length bit-string) 1) to 0
                when (char= #\1 value) collect (expt 2 pow-two))))

(defparameter oxygen-generator-rating (bit-string-to-integer (elt (reduce-sequency-via-decider 'decide-value-oxygen) 0)))

(defparameter inputcopy (copy-list *input3*))
(defparameter co2-scrubber-rating (bit-string-to-integer (elt (reduce-sequency-via-decider 'decide-value-co2) 0)))

(* oxygen-generator-rating co2-scrubber-rating)
