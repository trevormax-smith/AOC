(defparameter *testing* nil)
(defparameter *input7* (list))

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day7_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input7* (uiop:read-file-lines filename)))

(setf *input7* (map 'list 'parse-integer (str:split "," (elt *input7* 0))))

(defun find-distance (crab-positions position-guess)
  (reduce '+ (map 'list (lambda (crab) (abs (- crab position-guess))) crab-positions)))

(defun calc-fuel (crab-positions position-guess)
  (reduce '+ (map 'list
                  (lambda (crab)
                    (let ((diff (abs (- crab position-guess))))
                      (* (/ diff 2) (+ diff 1))))
                  crab-positions)))

(defun find-optimal-distance (crab-positions cost-function)
  (let ((best-guess 0)
        (best-distance 213131241241231231))
    (dotimes (this-guess (length crab-positions))
      (let ((this-distance (funcall cost-function crab-positions this-guess)))
            (if (< this-distance best-distance)
                (list (setf best-distance this-distance)
                      (setf best-guess this-guess)))))
    (return-from find-optimal-distance best-distance)))

(defparameter answer-a (find-optimal-distance *input7* 'find-distance))
(defparameter answer-b (find-optimal-distance *input7* 'calc-fuel))

(if *testing* (assert (= answer-a 37)) (print answer-a))
(if *testing* (assert (= answer-b 168)) (print answer-b))
