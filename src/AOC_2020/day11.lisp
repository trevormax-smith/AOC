(defparameter *input11* (uiop:read-file-lines "~/Documents/Lisp/input11"))

(defparameter height (length *input11*))
(defparameter width (length (first *input11*))) 

(setf *input11* (make-array (list height width)
                            :initial-contents *input11*))

(defparameter *last-state* *input11*)
(defparameter *current-state* *input11*)
(defparameter *index-array* 
  (loop for i0 from 0 to height collect 
        (loop for j0 from 0 to width collect 
              (list i0 j0))))

(defun get-item (i j)
  (cond ((or (= i -1) (= j -1))
         (values #\L))
        ((= i height)
         (values #\L))
        ((= j width)
         (values #\L))
        (t (aref *last-state* i j))))

(defun search-around-location (i j)
  (loop for i0 from (- i 1) to (+ i 1)
        append (loop 
                 for j0 from (- j 1) to (+ j 1)
                 when (not (equalp (list i j) (list i0 j0)))
                 collect (get-item i0 j0))))

(defun add-lists (a b)
  (mapcar '+ a b))

(defun look-for-next-seat (direction location)
  (do ((sight (add-lists direction location) (add-lists sight direction)))
      ((char/= #\. (apply 'get-item sight)) (apply 'get-item sight))))

(defun search-in-each-direction (i j)
  (loop for direction in 
        '((-1 -1) (0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0))
        collect (look-for-next-seat direction (list i j))))

(defun decide-next-state (i j search-function tolerance)
  (let ((seat (get-item i j))
        (neighbors (funcall search-function i j)))
    (cond
      ((char= seat #\.)
       (values seat))
      ((and (char= seat #\L) (= (count #\# neighbors) 0)) 
       (values #\#))
      ((and (char= seat #\#) (> (count #\# neighbors) tolerance)) 
       (values #\L))
      (t (values seat)))))

(defun build-next-state (search-function tolerance)
  (setf *last-state* *current-state*)
  (setf *current-state* 
        (loop for i from 0 below height collect
              (loop for j from 0 below width collect (decide-next-state i j search-function tolerance))))
  (setf *current-state* (make-array (list height width)
                                    :initial-contents *current-state*)))

(build-next-state #'search-around-location 3)

(do ()
    ((equalp *last-state* *current-state*) (values *current-state*))
    (build-next-state #'search-around-location 3))

(count #\# (aops:flatten *current-state*))

(defparameter *last-state* *input11*)
(defparameter *current-state* *input11*)
(defparameter *index-array* 
  (loop for i0 from 0 to height collect 
        (loop for j0 from 0 to width collect 
              (list i0 j0))))

(build-next-state #'search-in-each-direction 4)

(do ()
    ((equalp *last-state* *current-state*) (values *current-state*))
    (build-next-state #'search-in-each-direction 4))

(count #\# (aops:flatten *current-state*))
