(defparameter *input5* (uiop:read-file-lines "~/Documents/Lisp/AOC/AOC_2021/day5_input"))

(setf *input5* 
      (map 'list 
           (lambda (coords)
             (map 'list
                  (lambda (coord) (map 'list 'parse-integer
                                       (str:split "," coord)))
                  (str:split " -> " coords)))
           *input5*))

(defmacro apply-to-input5 (func)
  `(apply ,func (apply 'append (apply 'append *input5*))) )

(defparameter max-size (apply-to-input5 'max))

(incf max-size)

(defun make-floor-map ()
  (make-array (list max-size max-size) :initial-element 0 :element-type 'integer))

(defun add-one-to-floor-map (x y) (incf (aref floor-map x y)))

(defun general-range (n0 n1)
  (if (> n0 n1)
      (loop for n downfrom n0 to n1 collect n)
      (loop for n from n0 to n1 collect n)))

(defun add-line (coord-set part-2)
  (let ((x_0 (elt (elt coord-set 0) 0))
        (x_1 (elt (elt coord-set 1) 0))
        (y_0 (elt (elt coord-set 0) 1))
        (y_1 (elt (elt coord-set 1) 1)))
    (cond ((= x_0 x_1)
           (loop for y-coord in (general-range y_0 y_1) do
                 (add-one-to-floor-map x_0 y-coord)))
          ((= y_0 y_1)
           (loop for x-coord in (general-range x_0 x_1) do
                 (add-one-to-floor-map x-coord y_0))))
    (if (and part-2 (/= x_0 x_1) (/= y_0 y_1))
        (loop for x-coord in (general-range x_0 x_1)
              for y-coord in (general-range y_0 y_1) do
              (add-one-to-floor-map x-coord y-coord)))))

(defun >1 (num) (> num 1))

(defun solve-part (include-diagonals)
  (let ((floor-map (make-floor-map))
        (num-dangerous 0)
        (max-size-minus-1 (- max-size 1)))
    (map 'list (lambda (coord) (add-line coord include-diagonals)) *input5*)
    (loop for x from 0 to max-size-minus-1 do
          (loop for y from 0 to max-size-minus-1 do
                (when (>1 (aref floor-map x y)) (incf num-dangerous))))
    (print num-dangerous)))

(solve-part nil)
(solve-part t)
