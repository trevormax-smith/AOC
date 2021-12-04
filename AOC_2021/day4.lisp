(defparameter *input4* (uiop:read-file-lines "~/Documents/Lisp/AOC/AOC_2021/day4_input"))

(defparameter number-calls (pop *input4*))

(setf number-calls (map 'list 'parse-integer (str:split "," number-calls)))

(print number-calls)

(setf *input4* (remove "" *input4* :test #'string=))

(defun loop-row-collect-integers (row)
  (loop for i from 0 to 4
        collect (parse-integer (subseq row (* i 3) (if (< i 4) (* (+ i 1) 3) 14)))))

(defun get-cards (rows-list)
  (loop for i from 0 to (- (/ (length rows-list) 5) 1) 
  collect (map 'list 'loop-row-collect-integers (subseq rows-list (* i 5) (+ 5 (* i 5))))))

(setf *input4* (get-cards *input4*))

(defun cover-row-with-coin (num row)
  (let ((index (position num row)))
        (if (integerp index)
            (setf (elt row index) "O"))))

(defun cover-numbers-with-coins (num rows-list)
  (loop for board in rows-list do (mapc (lambda (row) (cover-row-with-coin num row)) board)))

(defun check-row-scores (board rownum)
  (every #'equal '("O" "O" "O" "O" "O") (elt board rownum)))

(defun check-col-scores (board colnum)
  (every #'equal '("O" "O" "O" "O" "O") (map 'list (lambda (row) (elt row colnum)) board)))

(defun check-board-for-bingo (board)
  (find 't (loop for i from 0 to 4
                 collect (or (check-col-scores board i)
                             (check-row-scores board i)))))

(defun check-boards (boards num)
  (cover-numbers-with-coins num boards)
  (loop for board in boards do (when (check-board-for-bingo board) (return board))))

(defparameter winning-number 0)

(defparameter number-calls-copy (copy-list number-calls))

(defun play-game ()
  (dolist (num number-calls)
    (let ((maybe-the-board (check-boards *input4* num)))
      (setf winning-number (pop number-calls))
      (if maybe-the-board (return-from play-game maybe-the-board)))))

(defparameter winner (play-game))

(print (* winning-number (apply '+ (remove "O" (apply #'append winner) :test #'equal))))

(defun reduce-boards (boards num)
  (cover-numbers-with-coins num boards)
  (remove-if 'check-board-for-bingo boards))

(defun play-game-2 ()
  (dolist (num number-calls)
    (setf *input4* (reduce-boards *input4* num))
    (setf winning-number (pop number-calls))
    (if (= 1 (length *input4*)) (return-from play-game-2 (check-boards *input4* 0)))))

(play-game-2)

(dolist (num number-calls)
  (setf winning-number (pop number-calls))
  (if (check-boards *input4* num) (return *input4*)))

(setf winner (elt *input4* 0))

(print (* winning-number (apply '+ (remove "O" (apply #'append winner) :test #'equal))))
