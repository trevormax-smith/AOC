(ql:quickload "uiop")

(defparameter *testing-a* nil)
(defparameter *testing-b* nil)
(defparameter *input* '())

(let ((filename "~/Documents/Lisp/AOC/src/AOC_2023/inputs/3"))
  (if *testing-a* (setf filename (concatenate 'string filename "_test_a")))
  (if *testing-b* (setf filename (concatenate 'string filename "_test_b")))
  (setf *input* (uiop:read-file-lines filename))
  (setf *input* (make-array
                  (list (length *input*) (length (first *input*)))
                  :initial-contents *input*)))

(defun array-map (function array
                  &optional (retval (make-array (array-dimensions array))))
  "Apply FUNCTION to each element of ARRAY.
Return a new array, or write into the optional 3rd argument."
  (dotimes (i (array-total-size array) retval)
    (setf (row-major-aref retval i)
          (funcall function (row-major-aref array i)))))

(defparameter *validarray*
  (make-array (array-dimensions *input*) :initial-element nil))

(defparameter *geararraylist* '())

(defparameter *gear-valid-map*
  (make-array '(3 7) :initial-contents
              '((nil nil T T T nil nil) (nil nil T T T nil nil) (nil nil T T T nil nil))))

(defun box-do (i j test)
  (loop for i-ind upfrom (- i 1) to (+ i 1)
        when (>= i 0)
        when (< i (array-dimension *input* 0))
        collect
        (loop for j-ind upfrom (- j 3) to (+ j 3)
              when (>= j 0)
              when (< j (array-dimension *input* 1))
              collect (if (funcall test i-ind j-ind)
                          (aref *input* i-ind j-ind)
                          #\.))))

(defun push-gear-box (i j arr)
  (let* ((gear-box (box-do i j (lambda (i-ind j-ind)
                                 (digit-char-p (aref *input* i-ind j-ind)))))
         (gear-array (make-array (list (length gear-box) (length (first gear-box)))
                                 :initial-contents gear-box)))
    (push gear-array *geararraylist*)))

(defun true-around (i j arr)
  "
  (-1 -1) (-1 0) (-1 1)
  (0  -1) (i  j) (0  1)
  (1  -1) (1  0) (1  1)
  "
  (loop for i-offset in '(-1 -1 -1 0 0 1 1 1)
        for j-offset in '(-1 -0 1 -1 1 -1 0 1)
        when (<= 0 (+ i i-offset))
        when (<= 0 (+ j j-offset))
        when (> (array-dimension arr 0) (+ i i-offset))
        when (> (array-dimension arr 1) (+ j j-offset))
        do (setf (aref arr (+ i i-offset) (+ j j-offset)) t))
  (setf (aref *input* i j) #\.))

(defun mark-valids (test fun arr)
  (loop for i from 0 below (array-total-size arr)
        for c = (row-major-aref arr i)
        when (funcall test c)
        do (funcall fun
                    (floor i (array-dimension arr 1))
                    (mod i (array-dimension arr 1))
                    arr))
  arr)

(defun access-array-row (arr i)
  (loop for c from 0 below (array-dimension arr 1)
        for index = (+ c (* i (array-dimension arr 1)))
        collect (row-major-aref arr index)))

(defun yank-first-numbers (input-string valid-list returnvals)
  "Recursively parse a row, replacing numbers with '.'s and storing
   valid numbers in return list just in case there are repeats."
  (let ((elements (remove ""
                          (uiop:split-string input-string
                                             :separator '(#\.))
                          :test 'equal)))
    (if (> (length elements) 0)
        (let*
          ((element (first elements))
           (ielement (search element input-string))
           (ielements (loop for i
                            from ielement
                            below (+ ielement (length element))
                            when (< i (array-dimension *input* 1))
                            collect i)))
          (loop for i in ielements
                with pushed = nil
                do (setf (elt input-string i) #\.)
                when (and (not pushed) (equal T (elt valid-list i)))
                do (push (parse-integer element) returnvals)
                (setf pushed t))
          (setf returnvals
                (yank-first-numbers input-string valid-list returnvals))))
    (values returnvals)))

(defun part-a () 
  (mark-valids
    (lambda (c) (and (char/= #\. c) (not (digit-char-p c))))
    'true-around *validarray*)
  (reduce '+
    (apply
      'concatenate 'list
      (loop for i from 0 below (array-dimension *input* 1)
            collect (yank-first-numbers
                      (concatenate 'string (access-array-row *input* i))
                      (access-array-row *validarray* i)
                      '())))))

(defun part-b ()
  (mark-valids (lambda (c) (char= #\* c)) 'push-gear-box *input*)
  ; (loop for i from 0 below (length *gearvalidlist*) do
        ; (setf (elt *gearvalidlist* i)
              ; (mark-valids
                ; (lambda (c) (equalp #\* c))
                ; 'true-around
                ; (array-map 'identity (elt *gearvalidlist* i)))))
  (let ((gears
          (loop for ga in *geararraylist*
                collect
                (apply 'concatenate 'list
                       (loop for i from 0 below (array-dimension ga 0)
                             for gearnums = (yank-first-numbers
                                              (concatenate 'string (access-array-row ga i))
                                              (access-array-row *gear-valid-map* i)
                                              '())
                             collect gearnums)))))
    (print gears)
    (apply '+
           (map 'list (lambda (x) (apply '* x))
                (remove-if-not (lambda (x) (= (length x) 2))
                               gears)))))

(if *testing-a*
    (let ((a-ans (part-a))) 
      (assert (= 4361 a-ans))
      (print a-ans))
    ;(print (part-a))
    )
(if *testing-b*
    (let ((b-ans (part-b)))
      (assert (= 467835 b-ans))
      (print b-ans))
    (print (part-b)))
