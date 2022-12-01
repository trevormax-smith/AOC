(ql:quickload "uiop")

(defparameter *testing* t)
(defparameter *input* (list (make-array '(0) :element-type 'integer :fill-pointer 0)))

(let ((filename "~/Documents/Lisp/AOC/AOC_2022/inputs/1"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (loop for row in (uiop:read-file-lines filename)
        do (if (string= row "")
               (push (make-array '(0) :element-type 'integer :fill-pointer 0) *input*)
               (vector-push-extend (parse-integer row) (car *input*)))))

(defun part-a ()
  ; map sum to input and return the biggest one
  ()
  )
