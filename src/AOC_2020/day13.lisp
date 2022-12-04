(defparameter *input13* (uiop:read-file-lines "~/Documents/Lisp/input13"))

(defparameter *departure* (parse-integer (first *input13*)))

(defparameter *allbusses* (uiop:split-string (first (last *input13*)) :separator ",")) 

(defparameter *busses* 
  (map 'vector 'parse-integer 
       (remove "x" *allbusses* :test 'string=)))

(defparameter *busses-and-positions*
  (map 'list (lambda (x) 
               (if (not (string= x "x"))
                   (list (parse-integer x) (position x *allbusses*)))) *allbusses*))

(setf *busses-and-positions* (remove nil *busses-and-positions*))

(defparameter *waits* (aops:each '- *busses* (map 'vector (lambda (x) (mod *departure* x)) *busses*)))

(multiple-value-setq (min-index minwait) (aops:argmin *waits*))

(print (* (elt *busses* min-index) minwait))

(defun make-pattern-2 (n0 n2 m0 m1)
  (let ((n-array (aops:generate (lambda (x) (+ n2 (* x n0)) ) (list m0) :position)))
    (loop for n being the elements of n-array when (= (mod (+ n m1) m0) 0) collect (list n (+ n m1)))))

(let ((counter 1) (offset 0))
  (dolist (n_entry *busses-and-positions*)
    (let ((result (first (first (make-pattern-2 counter offset (first n_entry) (elt n_entry 1))))))
      (setf offset result)
      (setf counter (* counter (first n_entry)))))
    (print offset))
