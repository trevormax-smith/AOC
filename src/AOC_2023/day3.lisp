(ql:quickload "uiop")

(defparameter *testing-a* nil)
(defparameter *testing-b* nil)
(defparameter *input* '())

(defun parse-draw (draw)
  (let ((picks (uiop:split-string draw :SEPARATOR '(#\, #\SPACE))))
    (loop for color in '("red" "green" "blue") collect
          (let ((val (position color picks :test 'string=)))
            (if (integerp val) (parse-integer (elt picks (- val 1))) 0)))))

(defun parse-row (row)
  (let ((game (uiop:split-string row :SEPARATOR '(#\:))))
    (list
      (parse-integer
        (elt (uiop:split-string
               (elt game 0) :SEPARATOR '(#\SPACE)) 1))
      (map 'list 'parse-draw
           (uiop:split-string (elt game 1) :SEPARATOR '(#\;))))))

(let ((filename "~/Documents/Lisp/AOC/src/AOC_2023/inputs/2"))
  (if *testing-a* (setf filename (concatenate 'string filename "_test_a")))
  (if *testing-b* (setf filename (concatenate 'string filename "_test_b")))
  (setf *input* (map 'list 'parse-row (uiop:read-file-lines filename))))

(defun draw-possible (draw)
  (and (<= (elt draw 0) 12)
       (<= (elt draw 1) 13)
       (<= (elt draw 2) 14)))

(defun power (draws)
  (apply '* 
         (loop for i from 0 to 2 collect 
               (apply 'max (map 'list (lambda (x) (elt x i)) draws)))))

(defun part-a () 
  (apply '+
         (loop for game in *input*
               when (every 'draw-possible (elt game 1))
               collect (elt game 0))))

(defun part-b ()
  (apply '+ (map 'list (lambda (x) (power (cadr x))) *input*)))

(if *testing-a*
    (let ((a-ans (part-a))) 
      (assert (= 8 a-ans))
      (print a-ans))
    (print (part-a)))
(if *testing-b*
    (let ((b-ans (part-b)))
      (assert (= 2286 b-ans))
      (print b-ans))
    (print (part-b)))
