(ql:quickload "uiop")
(ql:quickload "split-sequence")

(defparameter *testing* nil)
(defparameter *input* (list))

(defun read-in-char (c) (ecase c ((#\X #\A) 'r) ((#\Y #\B) 'p) ((#\Z #\C) 's)))

(let ((filename "~/Documents/Lisp/AOC/AOC_2022/inputs/2"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input* (mapcar (lambda (x) (list (read-in-char (elt x 0))
                                          (read-in-char (elt x 2))))
                        (uiop:read-file-lines filename))))

(defun play-round (throws)
  (let ((opp (car throws)) (you (cadr throws)))
    (ecase opp
      (R (ecase you (R 4) (P 8) (S 3)))
      (P (ecase you (R 1) (P 5) (S 9)))
      (S (ecase you (R 7) (P 2) (S 6))))))

(defun choose-throw (throws)
  (let ((opp (car throws)) (you (cadr throws)))
    (setf you (ecase opp
                (R (ecase you (R 'S) (P 'R) (S 'P)))
                (P (ecase you (R 'R) (P 'P) (S 'S)))
                (S (ecase you (R 'P) (P 'S) (S 'R)))))
    (list opp you)))

(defun part-a ()
  (reduce '+ (mapcar 'play-round *input*)))

(defun part-b ()
  (reduce '+ (mapcar 'play-round (mapcar 'choose-throw *input*))))

(let ((a-ans (part-a))
      (b-ans (part-b)))
  (if *testing*
      (assert (= 15 a-ans))
      (print a-ans))
  (if *testing*
      (assert (= 12 b-ans))
      (print b-ans)))
