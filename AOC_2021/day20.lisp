(ql:quickload "numcl")
(ql:quickload "array-operations")

(defpackage #:aoc_day20
  (:use :cl)
  (:shadowing-import-from #:numcl "MAKE-ARRAY" "ASARRAY" "RESHAPE" "+" "UNSTACK" "STACK"))
(in-package #:aoc_day20)

(defparameter *testing* nil)
(defparameter *input20* (make-hash-table :test 'equal))
(defparameter *enhancement-algorithm* nil)
(defparameter *image-height* 0)
(defparameter *image-width* 0)

(defmacro hash-coord (x y hash) `(gethash (list ,x ,y) ,hash 0))

(defparameter two-power-range-9 (numcl:expt 2 (numcl:arange 0 9)))

(defun get-enhancement (bit-vec)
  (let ((enh-ind (floor (numcl:sum (numcl:* (asarray bit-vec) two-power-range-9)))))
    (aref *enhancement-algorithm* enh-ind)))

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day20_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (loop for row in (uiop:read-file-lines filename)
        for index upfrom -2
        maximizing index into maxheight
        when (= -2 index)
          do (setf *enhancement-algorithm*
                   (asarray (map 'list (lambda (x) (if (char= x #\#) 1 0)) row)))
        when (>= index 0) do
        (loop for item across row 
              for column upfrom 0
              maximizing column into maxwidth do
              (setf (hash-coord index column *input20*)
                    (if (char= item #\#) 1 0))
              finally (setf *image-width* maxwidth))
        finally (setf *image-height* maxheight)))

(defun print-hash ()
  (loop for i upfrom -53 to (+ 53 *image-height*) do
        (loop with linestr = (make-array 0
                                         :element-type 'character
                                         :fill-pointer 0
                                         :adjustable t)
              for j upfrom -53 to (+ 53 *image-width*) do
              (vector-push-extend (if (= (hash-coord i j *input20*) 0) #\. #\#) linestr)
              finally (print (string linestr)))))

(defun get-output ()
  (let ((new-hash (make-hash-table :test 'equal)))
    (loop for i from -4 upto (+ 4 *image-height*) do
          (loop for j upfrom -4 to (+ 4 *image-height*) do 
                (loop with lookup-bitmap = '()
                      for ii upfrom (- i 1) to (+ i 1) do
                      (loop for jj upfrom (- j 1) to (+ j 1) do
                            (push (hash-coord ii jj *input20*) lookup-bitmap))
                      finally (setf (hash-coord i j new-hash) (get-enhancement (identity lookup-bitmap))))))
    (setf *input20* new-hash)))

(defparameter *n-enhancements* 0)

(defun count-pixels ()
  (loop for i from (- -4 *n-enhancements*) upto
        (apply '+ (list 52 *image-height* *n-enhancements*)) summing
        (loop for j from (- -4 *n-enhancements*) upto 
              (apply '+ (list 52 *image-width* *n-enhancements*)) counting
              (= 1 (hash-coord i j *input20*)) into pixels-2
              finally (return pixels-2))
        into pixels
        finally (return pixels)))

(defun solve-a ()
  (print-hash)
  (get-output)
  (print-hash)
  (get-output)
  (print-hash)
  (count-pixels))

(defun solve-b ()
  (loop for i upfrom 3 to 50 do
        (get-output)
        (print (list "got output!" i))
        when (= (mod i 25) 0) do (print-hash))
  (count-pixels))

(let ((ans-a (solve-a)))
  (if *testing* (assert (= ans-a 35)))
  (print ans-a))

(let ((ans-b (solve-b)))
  (if *testing* (assert (= ans-b 3351)))
  (print ans-b) *input20*)
