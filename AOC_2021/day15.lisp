(defparameter *testing* nil)
(defparameter *input15* nil)
(defparameter cost-map nil)
(defparameter unvisited nil)
(defparameter dims nil)
(defparameter target nil)

(ql:quickload "array-operations")

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day15_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input15*
        (map 'list
             (lambda (row) (map 'list 'digit-char-p row))
             (uiop:read-file-lines filename)))
  (setf *input15*
        (make-array (list (length *input15*) (length (elt *input15* 0)))
                    :initial-contents *input15*)))

(defun load-input ()
  (setf dims (array-dimensions *input15*))
  (setf cost-map
        (make-array dims
                    :initial-element 100000))
  (setf unvisited
        (make-array dims
                    :initial-element -1))
  (setf target (list (- (elt dims 0) 1) (- (elt dims 1) 1))))

(load-input)

(defun adjacent (i j)
  (list (list i (+ 1 j))
        (list (+ 1 i) j) 
        (list i (- j 1))
        (list (- i 1) j)))

(defun in-array (i j)
  (and (>= i 0) (>= j 0)
       (< i (elt dims 0))
       (< j (elt dims 1))))

(defun dijkstra-3 ()
  (loop for j from 0 upto (elt target 0) do
        (loop for i from 0 upto (elt target 1) do
          (defparameter cost (aref cost-map i j))
          (setf (aref unvisited i j) 0)
          (loop for (i j) in (adjacent i j)
                when (in-array i j)
                do (setf (aref cost-map i j)
                         (min (+ cost (aref *input15* i j))
                              (aref cost-map i j)))))))

(setf (aref cost-map 0 0) 0)
(dijkstra-3)
(aops:each (lambda (_) -1) unvisited)
(dijkstra-3)

(let ((ans-a (aref cost-map (elt target 0) (elt target 1))))
  (if *testing* (assert (= ans-a 40)))
  (print ans-a))

(let ((new-arrrrs '(1)))
  (loop for j below 5 do
        (let ((new-arrs '(0)))
          (loop for i below 5
                do (push (aops:vectorize (*input15*)
                           (if (> (+ i j *input15*) 9)
                               (+ i j *input15* -9)
                               (+ i j *input15*))) new-arrs))
          (setf new-arrs (reverse new-arrs))
          (push (apply 'aops:stack new-arrs) new-arrrrs)))
  (setf new-arrrrs (reverse new-arrrrs))
  (setf *input15* (apply 'aops:stack new-arrrrs)))

(load-input)
(setf (aref cost-map 0 0) 0)

(time 
  (progn (dijkstra-3)
         (aops:each (lambda (x) -1) unvisited)
         (dijkstra-3)
         (aops:each (lambda (x) -1) unvisited)
         (dijkstra-3)))

(let ((ans-b (aref cost-map (elt target 0) (elt target 1))))
  (if *testing* (assert (= ans-b 315)))
  (print ans-b))
