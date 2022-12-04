(defparameter *input10* (uiop:read-file-lines "~/Documents/Lisp/input10"))

(setf *input10* (map 'list (lambda (x)
             (parse-integer (string x))) *input10*))

(setf *input10* (append (list 0 (+ 3 (apply 'max *input10*))) *input10*))

(setf *input10* (sort *input10* '<))

(defparameter *differences* 
  (loop for low in (subseq *input10* 0 (- (length *input10*) 1))
        for hi in (subseq *input10* 1)
        collect (- hi low)))

(* (count 1 *differences*) (count 3 *differences*))

(defparameter ones-to-steps '(1 1 2 4))

(dotimes (i 50)
  (let ((j (length ones-to-steps)))
        (setf ones-to-steps 
              (append ones-to-steps (list (apply '+ (subseq ones-to-steps (- j 3) j)))))))

(defun possible-steps-for-num-ones (i)
  (elt ones-to-steps i))

(defun step-through (-sequence-)
  (if (> (length -sequence-) 0)
  (let ((i (count 1 (subseq -sequence- 0 (position 3 -sequence-)))))
    (append (list (possible-steps-for-num-ones i)) (step-through (subseq -sequence- (max 1 i)))))))

(defparameter -nodes- (step-through *differences*))

(apply '* -nodes-)
