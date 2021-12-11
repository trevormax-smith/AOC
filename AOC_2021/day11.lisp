(ql:quickload :select)

(defparameter *testing* nil)

(defun load-input ()
  (let ((filename "~/Documents/Lisp/AOC/AOC_2021/day11_input"))
    (if *testing* (setf filename (concatenate 'string filename "_test")))
    (setf *input11* (uiop:read-file-lines filename)))
  (setf *input11* (map 'list (lambda (row) (map 'list 'digit-char-p row)) *input11*))
  (defparameter *input-shape* (list (length *input11*) (length (elt *input11* 0))))
  (setf *input11* (make-array *input-shape* :initial-contents *input11*)))

(defparameter *input11* (load-input))

(defun make-popped-layer ()
  (make-array '(10 10) :element-type 'boolean :initial-element nil))

(defparameter popped-layer (reset-popped-layer))

(defun add-1 ()
  (loop for i from 0 to 9 do
        (loop for j from 0 to 9 do (incf (aref *input11* i j)))))

(defmacro loop-check-do (conditional func)
  `(loop for i from 0 to 9 do
         (loop for j from 0 to 9
               when (funcall ,conditional i j)
               do (funcall ,func i j))))

(defun incf-around-ten (i j)
  (loop for i0 from (- i 1) to (+ i 1)
        when (and (<= 0 i0) (<= i0 9))
        do (loop for j0 from (- j 1) to (+ j 1)
                 when (and (<= 0 j0) (<= j0 9))
                 do (incf (aref *input11* i0 j0))))
  (pop-tens))

(defun check-gte-ten (i j) (<= 10 (aref *input11* i j)))

(defun found-ten (i j)
 (if (not (aref popped-layer i j))
     (list (setf (aref popped-layer i j) t) 
           (incf-around-ten i j))))

(defun found-popped (i j)
  (setf (aref *input11* i j) 0)
  (incf num-flashes))

(defun pop-tens ()
  (loop-check-do 'check-gte-ten 'found-ten))

(defun reset-tens ()
  (loop-check-do 'check-gte-ten 'found-popped))

(defparameter num-flashes 0)

(defun iterate-steps (part-b)
  (dotimes (stp 1000)
    (add-1) (pop-tens)
    (if part-b (let ((found-answer t))
                 (loop-check-do 'didnt-pop
                                (lambda (_i _j) (setf found-answer nil)))
                 (if found-answer
                     (return-from iterate-steps (incf stp))))
        (if (= stp 100) (return-from iterate-steps num-flashes)))
    (setf popped-layer (make-popped-layer)) (reset-tens)))

(defun didnt-pop (i j) (not (aref popped-layer i j)))

(let ((ans-a (iterate-steps nil)))
  (if *testing* (assert (= ans-a 1656)))
  (print ans-a))

(setf *input11* (load-input))

(let ((ans-b (iterate-steps t)))
  (if *testing* (assert (= ans-b 195)))
  (print ans-b))
