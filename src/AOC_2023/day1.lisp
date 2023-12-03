(ql:quickload "uiop")

(defparameter *testing-a* nil)
(defparameter *testing-b* nil)
(defparameter *input* '())

(let ((filename "~/Documents/Lisp/AOC/src/AOC_2023/inputs/1"))
  (if *testing-a* (setf filename (concatenate 'string filename "_test_a")))
  (if *testing-b* (setf filename (concatenate 'string filename "_test_b")))
  (setf *input* (uiop:read-file-lines filename)))

(defun get-int-locs (row)
  (loop for c across row
        for i upfrom 0
        when (parse-integer (string c) :JUNK-ALLOWED t)
        collect (list (parse-integer (string c) :JUNK-ALLOWED t) i)))

(defparameter fancy-int-pairs
  '(("one" 1)
    ("two" 2)
    ("three" 3)
    ("four" 4)
    ("five" 5)
    ("six" 6)
    ("seven" 7)
    ("eight" 8)
    ("nine" 9)))

(defun find-fancy-int (row fancy-int)
  (let ((intstr (first fancy-int))
        (intval (cadr fancy-int))
        (strlen (length (first fancy-int))))
    (loop for i from 0 to (- (length row) strlen)
          when (string= intstr (subseq row i (+ i strlen)))
          collect (list intval i))))

(defun get-fancy-ints (row)
  (apply 'concatenate 'list
         (loop for fancy-int in fancy-int-pairs
               collect (find-fancy-int row fancy-int))))

(defun get-all-ints (row)
  (sort (concatenate 'list (get-fancy-ints row) (get-int-locs row))
        '<
        :key 'cadr))

(defun first-last (digits &optional (lam (lambda (x) x)))
  (+ (funcall lam (first (last digits)))
     (* 10 (funcall lam (first digits)))))

(defun part-a () 
  (apply '+ (map 'list 'first-last (map 'list 'get-ints *input*))))

(defun part-b ()
  (apply '+ (map 'list (lambda (x) (first-last x 'car))
                 (map 'list 'get-all-ints *input*))))

(if *testing-a*
    (let ((a-ans (part-a))) 
      (assert (= 142 a-ans))
      (print a-ans))
    (print (part-a)))
(if *testing-b*
    (let ((b-ans (part-b)))
      (assert (= 281 b-ans))
      (print b-ans))
    (print (part-b)))
