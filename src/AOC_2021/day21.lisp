(ql:quickload "numcl")
(ql:quickload "array-operations")

(defpackage #:aoc_day21
  (:use :cl)
  (:shadowing-import-from #:numcl "ASARRAY"))
(in-package #:aoc_day21)

(defparameter *testing* t)
(defvar dice1 nil)
(defvar player1 nil)
(defvar player2 nil)

(defun practice-dice ()
  (let ((roll -1)
        (n-rolls 0))
    (lambda (&optional (get-rolls nil))
      (if get-rolls n-rolls
          (progn
            (incf n-rolls)
            (+ 1 (mod (incf roll) 100)))))))

(defun player (initial)
  (let ((player-position initial)
        (score 0))
    (lambda (roll)
      (if (= roll -1) score
          (progn
            (setf player-position (+ 1 (mod (+ (- player-position 1) roll) 10)))
            (incf score player-position))))))

(setf dice1 (practice-dice))
(setf player1 (player (if *testing* 4 8)))
(setf player2 (player (if *testing* 8 3)))

(defun roll3 ()
  (+ (funcall dice1) (funcall dice1) (funcall dice1)))

(defun check-score (score1 score2)
  (if (or (>= score1 1000) (>= score2 1000))
      (* (min score1 score2) (funcall dice1 t))
      nil))

(defun playgame ()
  (loop for i upfrom 0 do
        (let* ((roll1 (roll3))
               (score1 (funcall player1 roll1))
               (score2 (funcall player2 -1))
               (gameover (check-score score1 score2)))
          (if gameover (return-from playgame gameover)
              (let* ((roll2 (roll3))
                     (score2 (funcall player2 roll2))
                     (gameover (check-score score1 score2)))
                (if gameover (return-from playgame gameover)))))))

(defun solve-a ()
  (playgame))

(let ((ans-a (solve-a)))
  (if *testing* (assert (= ans-a 739785)))
  (print ans-a))

(defparameter dice-rolls
  (loop with vals = '()
        for i from 1 to 3 do
        (loop for j from 1 to 3 do
              (loop for k from 1 to 3 do
                    (push (+ i j k) vals)))
        finally (return
                  (list
                    (list 3 (count 3 vals))
                    (list 4 (count 4 vals))
                    (list 5 (count 5 vals))
                    (list 6 (count 6 vals))
                    (list 7 (count 7 vals))
                    (list 8 (count 8 vals))
                    (list 9 (count 9 vals))))))

(defparameter potential-points (make-hash-table))
(defun player-potential-points (-position-)
  (multiple-value-bind (value n)
    (gethash -position- potential-points
             (loop for (roll freq) integer in dice-rolls
                   collect (list (+ 1 (mod (+ (- -position- 1) roll) 10)) freq)))
    (if n nil)
    value))

(defvar my-board-state (lambda () ))

(defun hashed-evolve-board-state ()
  (let ((state-outcome 'state-outcome)
        (dump 'dump)
        (board-state-hash (make-hash-table :test 'equal)))
    (setf dump
          (lambda () (loop for key being the hash-keys in board-state-hash
                           using (hash-value v) do
                            (format t "~a => ~a~%" key v))))
    (setf state-outcome
          (lambda (current-player-points
                    other-player-points
                    current-player-position
                    other-player-position
                    &optional (do-dump nil))
            (if do-dump (funcall dump))
            (multiple-value-bind
              (player-universe-wins computed)
              (gethash (list current-player-points
                             other-player-points
                             current-player-position
                             other-player-position) board-state-hash)
              (if computed player-universe-wins
                  (loop with current-player-wins = (asarray '(0 0) :type 'integer)
                        for (new-pos freq) in
                        (player-potential-points current-player-position) do
                        (let ((player-points (+ current-player-points new-pos)))
                          (if (>= player-points 21)
                              (numcl:incf (numcl:aref current-player-wins 0) freq)
                              (setf current-player-wins
                                (numcl:+
                                  current-player-wins
                                  (numcl:*
                                    freq
                                    (asarray
                                      (numcl:reverse
                                        (funcall state-outcome
                                                 other-player-points
                                                 player-points
                                                 other-player-position
                                                 new-pos))))))))
                          finally
                          (return
                            (setf (gethash (list current-player-points
                                                 other-player-points
                                                 current-player-position
                                                 other-player-position)
                                           board-state-hash) current-player-wins)))))))))

(setf my-board-state (hashed-evolve-board-state))

(let ((ans-b (numcl:amax (funcall my-board-state 0 0
                                  (if *testing* 4 8)
                                  (if *testing* 8 3)))))
  (if *testing* (assert (= ans-b 444356092776315)))
  (print ans-b))
