(ql:quickload "numcl")
(ql:quickload "array-operations")

(defpackage #:aoc_day21
  (:use :cl)
  (:shadowing-import-from #:numcl "ASARRAY"))
(in-package #:aoc_day21)

(defparameter *testing* nil)
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
  (gethash -position- potential-points
           (loop for (roll freq) integer in dice-rolls
                 collect (list (+ 1 (mod (+ (- -position- 1) roll) 10)) freq))))

(defun board-state ()
  (defparameter board-state-hash (make-hash-table :test 'equal))
  (lambda  (player1-points
            player2-points
            player1-position
            player2-position
            turn-1->0-2->1
            roll)
    (multiple-value-bind
      (ans exists) (gethash
                     (list player1-points player2-points 
                           player1-position player2-position
                           turn-1->0-2->1 roll)
                     board-state-hash)
      (if exists ans
          (loop for (new-position freq) integer in dice-rolls do
                (if (>= (+ new-position (if turn-1->0-2->1 player2-points player1-points)) 21)
                freq
            (* freq
               board-state (+ player1-points (if turn-1->0-2->1 0 new-position))
               (+ player2-points (if turn-1->0-2->1 new-position 0))
               (if turn-1->0-2->1 player1-position new-position)
               (if turn-1->0-2->1 new-position player2-position)
               (- 1 turn-1->0-2->1)))
              )
            
          
          )
      )

    
    
    )
  (if exists
      ))

#|
(let ((ans-b (solve-b)))
  (if *testing* (assert (= ans-b 3351)))
  (print ans-b) *input20*)
|#
