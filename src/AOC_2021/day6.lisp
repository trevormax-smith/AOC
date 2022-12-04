(defparameter *input6* (uiop:read-file-lines "~/Documents/Lisp/AOC/AOC_2021/day6_input"))

(setf *input6* (map 'list 'parse-integer (str:split "," (elt *input6* 0))))

(defparameter *fish-timers* (make-hash-table))

(mapc (lambda (key) (incf (gethash key *fish-timers* 0))) *input6*)

(defun decrement-group-days (days-remaining)
  (if (= days-remaining 0)
      (list (incf (gethash 7 *fish-timers* 0) (gethash 0 *fish-timers* 0))
            (incf (gethash 9 *fish-timers* 0) (gethash 0 *fish-timers* 0)))
      (incf (gethash (- days-remaining 1) *fish-timers* 0)
            (gethash days-remaining *fish-timers* 0)))
  (setf (gethash days-remaining *fish-timers* 0) 0))

(defun pass-a-day () (dotimes (day 10) (decrement-group-days day)))

(defun loop-some-days (days) (dotimes (day days) (pass-a-day)))

(defun sum-fish ()
  (let ((num-fish 0))
    (dotimes (days-remaining 9) (incf num-fish (gethash days-remaining *fish-timers*)))
    (return-from sum-fish num-fish)))

(list (loop-some-days 80) (print (sum-fish)))

(list (loop-some-days (- 256 80)) (print (sum-fish)))
