(ql:quickload "numcl")
(ql:quickload "array-operations")

(defpackage #:aoc_day20
  (:use :cl)
  (:shadowing-import-from #:numcl "MAKE-ARRAY" "ASARRAY" "RESHAPE" "+" "UNSTACK" "STACK"))
(in-package #:aoc_day20)

(defparameter *testing* nil)
(defparameter *input20* nil)
(defparameter *enhancement-algorithm* nil)
(defparameter *image-height* 0)
(defparameter *image-width* 0)

(defparameter odd-toggle 0)

(defmacro array-coord (x y array) `(numcl:aref array (list ,x ,y)))

(defparameter two-power-range-9 (numcl:expt 2 (numcl:arange 0 9)))

(defun get-enhancement (bit-vec)
  (let ((enh-ind (floor (numcl:sum (numcl:* (asarray bit-vec) two-power-range-9)))))
    (aref *enhancement-algorithm* enh-ind)))

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day20_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (loop with my-array = '()
        for row in (uiop:read-file-lines filename)
        for index upfrom -2
        maximizing index into maxheight
        when (= -2 index)
          do (setf *enhancement-algorithm*
                   (asarray (map 'list (lambda (x) (if (char= x #\#) 1 0)) row)))
        when (>= index 0) do
        (push (loop with newrow = '()
                    for item across row
                    for column upfrom 0
                    maximizing column into maxwidth do
                    (push (if (char= item #\#) 1 0) newrow)
                    finally (setf *image-width* maxwidth)
                    finally (reverse (return newrow))) my-array)
        finally (setf *image-height* maxheight)
        finally (setf *input20* (asarray (reverse my-array)))))

(defun print-array ()
  (loop for row in (unstack *input20*) do (print row)))

(defun get-output ()
  (loop with lookup-bitmap = '()
        for ii upfrom (- i 1) to (+ i 1) do
        (loop for jj upfrom (- j 1) to (+ j 1) do
              
              )
        ))

(defparameter *n-enhancements* 0)

(defun count-pixels ()
  (numcl:s))

(defun solve-a ()
  (print-array)
  (get-output)
  (print-array)
  (get-output)
  (print-array)
  (count-pixels))

(defun solve-b ()
  )

#|
(let ((ans-a (solve-a)))
  (if *testing* (assert (= ans-a 35)))
  (print ans-a))

(let ((ans-b (solve-b)))
  (if *testing* (assert (= ans-b 3351)))
  (print ans-b) *input20*)
|#
