(ql:quickload :select)
(ql:quickload :str)
(ql:quickload :vectors)
(ql:quickload :array-operations)

(defparameter *testing* nil)

(defun load-input ()
  (let ((filename "~/Documents/Lisp/AOC/AOC_2021/day13_input"))
    (if *testing* (setf filename (concatenate 'string filename "_test")))
    (setf *input13* (uiop:read-file-lines filename))))

(defparameter *input13* (load-input))

(defparameter coords (subseq *input13*
                             0 (position "" *input13* :test #'equal)))
(defparameter folds (subseq *input13*
                            (+ 1 (position "" *input13* :test #'equal))))

(setf coords (map 'list (lambda (coord)
                          (map 'list 'parse-integer (str:split "," coord)))
                  coords))

(setf folds (map 'list (lambda (fold) (elt (str:split " " fold) 2)) folds))
(setf folds (map 'list (lambda (fold) (str:split "=" fold)) folds))

(defparameter shape-x
   (reduce 'max (map 'list (lambda (coord) (elt coord 0)) coords)))
(defparameter shape-y
   (reduce 'max (map 'list (lambda (coord) (elt coord 1)) coords)))

(defparameter arr (make-array (list (incf shape-y) (incf shape-x))
                              :element-type 'integer
                              :initial-element 0))

#| line 36 and the input is all set up finally... |#

(defun add-coord (coord)
  (incf (select:select arr (elt coord 1) (elt coord 0))))

(mapc 'add-coord coords)

(defun perform-x-fold (line)
  (loop for top from 0 below line
        for bottom downfrom (* 2 line) above line
        collect (vec:v+
                  (select:select arr t top)
                  (select:select arr t bottom)) into vectors
        finally (return vectors)))

(defun make-array-x-fold (vectors)
  (let ((new-arr (make-array (list (length vectors) shape-y)
                            :initial-contents vectors)))
    (aops:each-index (i j) (aref new-arr j i))))

(defun perform-y-fold (line)
  (loop for top from 0 below line
        for bottom downfrom (* 2 line) above line
        collect (vec:v+
                  (select:select arr top t)
                  (select:select arr bottom t)) into vectors
        finally (return vectors)))

(defun make-array-y-fold (vectors)
  (make-array (list (length vectors) shape-x)
              :initial-contents vectors))

(defun make-fold (fold)
  (let ((line (parse-integer (elt fold 1))))
    (defparameter new-arr (if (string= (elt fold 0) "x")
                              (make-array-x-fold (perform-x-fold line))
                              (make-array-y-fold (perform-y-fold line))))
    (setf shape-x (array-dimension new-arr 1))
    (setf shape-y (array-dimension new-arr 0))
    (setf arr new-arr)))

(let ((answer-a 0))
  (make-fold (elt folds 0))
  (aops:each-index (i j) (if (< 0 (aref arr i j)) (incf answer-a)))
  (if *testing* (assert (= answer-a 17)))
  (print answer-a))

(mapc 'make-fold (subseq folds 1))

(aops:each-index (i j) (if (< 0 (aref arr i j)) '0 '-))
