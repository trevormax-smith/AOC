(defparameter *input6* (uiop:read-file-lines "~/Documents/Lisp/input6"))

(setf *input6* (map 'list 'sb-unicode:words (zip *input6*)))

(defun words (answers)
  (remove nil (map 'list (lambda (ans) (if (position #\  ans) nil ans)) answers)))

(setf *input6* (map 'list 'words *input6*))

(defun count-all-letter (answers letter)
  (every (lambda (ans) (position letter ans)) answers))

(defun check-each-letter (answers)
  (count t (map 'list (lambda (letter) (count-all-letter answers letter)) "abcdefghijklmnopqrstuvwxyz")))
