(ql:quickload "uiop")

(defparameter *testing* nil)
(defparameter *input* (list (make-array '(0) :element-type 'integer :fill-pointer 0)))

(let ((filename "~/Documents/Lisp/AOC/AOC_2022/inputs/1"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (loop for row in (uiop:read-file-lines filename)
        do (if (string= row "")
               (push (make-array '(0) :element-type 'integer :fill-pointer 0) *input*)
               (vector-push-extend (parse-integer row) (car *input*)))))

(defun +-reduce (li)
  (reduce '+ li))

(defun map-sum (li)
  (map 'list '+-reduce li))

(defun part-a ()
  (apply 'max (map-sum *input*)))

(defun part-b ()
  (+-reduce (subseq (sort (map-sum *input*) '>) 0 3)))

(let ((a-ans (part-a))
      (b-ans (part-b)))
  (if *testing*
      (assert (= 24000 a-ans))
      (print a-ans))
  (if *testing*
      (assert (= 45000 b-ans))
      (print b-ans)))
