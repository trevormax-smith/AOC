(defparameter *testing* nil)
(defparameter *input9* (list))

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day9_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input9* (uiop:read-file-lines filename)))

(setf *input9* (map 'list (lambda (row) (map 'list 'digit-char-p row)) *input9*))

(defparameter *input-shape* (list (length *input9*) (length (elt *input9* 0))))

(setf *input9*
      (make-array *input-shape* 
                  :initial-contents *input9*))

(defun try-access (x_ind y_ind)
  (if (or (> 0 x_ind) (<= (elt *input-shape* 0) x_ind))
      (return-from try-access nil))
  (if (or (> 0 y_ind) (<= (elt *input-shape* 1) y_ind))
      (return-from try-access nil))
  (aref *input9* x_ind y_ind))

(defun check-around-index (x_ind y_ind)
  (let ((arounds (list)))
    (loop for x_offs in '(-1 0 1 0) 
          for y_offs in '(0 1 0 -1)
          do (push (try-access (+ x_ind x_offs) (+ y_ind y_offs)) arounds))
    (return-from check-around-index (remove nil arounds))))

(defun local-minima-p (x_ind y_ind)
  (if (not (try-access x_ind y_ind)) (return-from local-minima-p nil))
  (if (= 9 (try-access x_ind y_ind)) (return-from local-minima-p nil))
  (loop for around in (check-around-index x_ind y_ind)
        when (< around (aref *input9* x_ind y_ind))
        do (return-from local-minima-p nil))
  t)

(defun incf-my-int (int) (incf int))

(defun not-eq-9 (val) (= val 9))

(defparameter basin-sum 0)

(defun find-basin (x_ind y_ind)
  (setf (aref *input9* x_ind y_ind) 9)
  (loop for x_offs in '(-1 0 1 0) 
        for y_offs in '(0 1 0 -1)
        when (local-minima-p (+ x_ind x_offs) (+ y_ind y_offs))
        do (list 
             (incf basin-sum)   
             (find-basin (+ x_ind x_offs) (+ y_ind y_offs)))))

(defun part (part-a)
  (let ((return-vals (list)))
    (loop for x_ind from 0 to (- (elt *input-shape* 0) 1)  do
          (loop for y_ind from 0 to (- (elt *input-shape* 1) 1)
                when (local-minima-p x_ind y_ind)
                do 
                (if part-a
                    (push (+ 1 (try-access x_ind y_ind)) return-vals)
                    (list
                      (incf basin-sum)
                      (find-basin x_ind y_ind)
                      (push basin-sum return-vals)
                      (setf basin-sum 0)))))
    (print return-vals)
    (if part-a
        (return-from part (reduce '+ return-vals))
        (return-from part (reduce '* (subseq (sort return-vals '>) 0 3))))))

(defparameter *answer-a* (part t))

(if *testing* (assert (= *answer-a* 15)) (print *answer-a*))

(defparameter *answer-b* (part nil))

(if *testing* (assert (= *answer-b* 1134)) (print *answer-b*))
