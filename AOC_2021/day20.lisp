(ql:quickload "numcl")

(defpackage #:aoc_day20
  (:use :cl)
  (:shadowing-import-from #:numcl "MAKE-ARRAY" "ASARRAY" "RESHAPE" "+" "UNSTACK" "STACK" "AREF" "ARANGE"))
(in-package #:aoc_day20)

(defparameter *testing* nil)
(defparameter *input20* nil)
(defparameter *enhancement-algorithm* nil)
(defparameter *image-height* 0)
(defparameter *image-width* 0)
(defparameter two-power-range-9 (numcl:expt 2 (numcl:arange 0 9)))
(defparameter *n-enhancements* 0)

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
                    finally (return (reverse newrow))) my-array)
        finally (setf *image-height* maxheight)
        finally (setf *input20* (asarray (reverse my-array)))))

(defun print-array (&optional (ar *input20*))
  (loop for row in (unstack ar) do (terpri) (map 'list 'prin1 row)))

(defun get-output ()
  (loop with lookup-bitmap-layers = '()
        with curwidth = (array-dimension *input20* 0)
        with curheight = (array-dimension *input20* 1)
        for ii upfrom 0 to 2 do
        (loop for jj upfrom 0 to 2 do
              (let ((new-array (asarray (numcl:make-array
                                          (list (+ 4 curwidth) (+ 4 curheight))
                                          :initial-element (mod *n-enhancements* 2)))))
                (setf (aref new-array
                            `(,(+ ii 1) ,(+ ii curwidth 1))
                            `(,(+ jj 1) ,(+ jj curheight 1)))
                      *input20*)
                (push new-array lookup-bitmap-layers)))
        finally (setf *input20*
                      (aref (numcl:map-array (lambda (x) (elt *enhancement-algorithm* (floor x)))
                                         (numcl:sum
                                           (numcl:*
                                             (stack (reverse lookup-bitmap-layers) :axis 0)
                                             (numcl:reshape two-power-range-9 '(9 1 1)))
                                           :axes 0)) '(1 -1) '(1 -1))))
  (print *n-enhancements*)
  (incf *n-enhancements*))

(defun count-pixels ()
  (numcl:sum *input20*))

(defun solve-a ()
  (get-output)
  (get-output)
  (count-pixels))

(defun solve-b ()
  (loop for i upfrom 3 to 50 do (get-output))
  (count-pixels))

(let ((ans-a (solve-a)))
  (if *testing* (assert (= ans-a 35))
      (assert (= ans-a 5425)))
  (print ans-a))

(let ((ans-b (solve-b)))
  (if *testing* (assert (= ans-b 3351)))
  (print ans-b) *input20*)
