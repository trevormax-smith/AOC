(ql:quickload "uiop")
(ql:quickload "split-sequence")

(defparameter *testing* nil)
(defparameter *input* (list))

(let ((filename "~/Documents/Lisp/AOC/AOC_2022/inputs/3"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input* (mapcar (lambda (x) (map 'list 'identity x)) (uiop:read-file-lines filename))))

(defun get-compartments (rucksack)
  (let ((half (/ (length rucksack) 2)))
    (list (subseq rucksack 0 half) (subseq rucksack half))))

(defun get-groups ()
  (loop for i from 0 below (/ (length *input*) 3)
        collect (subseq *input* (* i 3) (* (+ i 1) 3))))

(defun find-misplaced (rucksack)
  (car (intersection (car rucksack) (cadr rucksack))))

(defun find-badge (group)
  (car
    (intersection
      (intersection (car group) (cadr group))
      (caddr group))))

(defun score-priority (item)
  (let ((ascii (char-code item)))
    (if (>= ascii 97) (- ascii 96) (- ascii 38))))

(defun part-a ()
  (reduce '+ (mapcar 'score-priority
                     (mapcar 'find-misplaced
                             (mapcar 'get-compartments *input*)))))

(defun part-b ()
  (reduce '+  (mapcar 'score-priority
          (mapcar 'find-badge (get-groups)))))

(let ((a-ans (part-a))
      (b-ans (part-b)))
  (if *testing*
      (assert (= 157 a-ans))
      (print a-ans))
  (if *testing*
      (assert (= 70 b-ans))
      (print b-ans)))
