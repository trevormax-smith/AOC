(defparameter *testing* nil)
(defparameter *input10* (list))

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day10_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input10* (uiop:read-file-lines filename)))

(defparameter row-stack (list))

(defparameter open-delimeters (list #\[ #\{ #\< #\())
(defparameter close-delimeters (list #\] #\} #\> #\)))

(defun get-match (delimeter)
  (cond ((find delimeter open-delimeters)
         (elt close-delimeters (position delimeter open-delimeters)))
        ((find delimeter close-delimeters)
         (elt open-delimeters (position delimeter close-delimeters)))))

(defun check-if-close-illegal (close-delimeter)
  (if (char/= close-delimeter (pop row-stack))
      (return-from check-if-close-illegal t)
      (return-from check-if-close-illegal nil)))

(defun parse-row (row)
  (loop for chunk-delimeter across row
        when (find chunk-delimeter open-delimeters)
        do (push (get-match chunk-delimeter) row-stack)
        when (find chunk-delimeter close-delimeters)
        do (if (check-if-close-illegal chunk-delimeter)
               (return-from parse-row chunk-delimeter))))

(defun points (ch)
  (cond ((equal nil ch) 0)
        ((char= #\) ch) 3)
        ((char= #\] ch) 57)
        ((char= #\} ch) 1197)
        ((char= #\> ch) 25137)))

(defun part-a ()
  (let ((part-a-points 0))
    (dolist (row *input10*)
      (setf row-stack (list))
      (incf part-a-points (points (parse-row row))))
    (values part-a-points)))

(defparameter total-score 0)

(defun points-2 (ch)
  (setf total-score (* total-score 5))
  (incf total-score 
        (cond ((equal nil ch) 0)
              ((char= #\) ch) 1)
              ((char= #\] ch) 2)
              ((char= #\} ch) 3)
              ((char= #\> ch) 4))))

(defparameter row-scores (list))

(defun parse-collect (row)
  (setf row-stack (list))
  (setf total-score 0)
  (parse-row row)
  (mapc 'points-2 row-stack)
  (push total-score row-scores))

(defun part-b ()
  (setf *input10* (remove-if 'parse-row *input10*))
  (mapc 'parse-collect *input10*)
  (setf row-scores (sort row-scores '<))
  (values (elt row-scores (- (/ (length row-scores) 2) 1/2))))

(let ((answer-part-a (part-a)))
  (if *testing* (assert (= answer-part-a 26397)))
  (print answer-part-a))

(let ((answer-part-b (part-b)))
  (if *testing* (assert (= answer-part-b 288957)))
  (print answer-part-b))
