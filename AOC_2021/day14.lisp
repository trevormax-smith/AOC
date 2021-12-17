(load "~/quicklisp/setup.lisp")
(ql:quickload :str)

(defparameter *testing* t)

(defun load-input ()
  (let ((filename "~/Documents/Lisp/AOC/AOC_2021/day14_input"))
    (if *testing* (setf filename (concatenate 'string filename "_test")))
    (setf *input14* (uiop:read-file-lines filename))))

(defparameter *input14* (load-input))

(defparameter insertion-rules (subseq *input14* 2))

(defparameter polymer (elt *input14* 0))

(setf insertion-rules
      (map 'list (lambda (rule)
                   (list (subseq rule 0 2)
                         (coerce (list (elt rule 0) (elt rule 6)) 'string)
                         (coerce (list (elt rule 6) (elt rule 1)) 'string)))
           insertion-rules))

(defparameter insertion-rules-hash (make-hash-table :test #'equal))
(mapc (lambda (x) (setf (gethash (elt x 0) insertion-rules-hash)
                        (subseq x 1))) insertion-rules)

(defparameter index-polymer
  (map 'list
       (lambda (x y) (concatenate 'string (list x y)))
       (subseq polymer 0 (- (length polymer) 1))
       (subseq polymer 1)))

(defparameter pair-hash (make-hash-table :test #'equal))

(defun incf-hash (pair &optional (by 1) (hash pair-hash))
  (incf (gethash pair hash 0) by))

(mapc 'incf-hash index-polymer)

(defun perform-insertions (times)
  (dotimes (x times)
    (let ((new-hash (make-hash-table :test #'equal)))
      (maphash
        (lambda (pair val)
          (progn
            (incf-hash (elt (gethash pair insertion-rules-hash) 0) val new-hash)
            (incf-hash (elt (gethash pair insertion-rules-hash) 1) val new-hash)))
        pair-hash)
      (setf pair-hash new-hash))))

(defun part-a (insertions)
  (perform-insertions insertions)
  (let ((count-hash (make-hash-table :test 'equal)))
    (maphash (lambda (ch v) (progn (incf-hash (elt ch 0) v count-hash)
                                   (incf-hash (elt ch 1) v count-hash))) pair-hash)
    (incf-hash (elt polymer 0) -1 count-hash)
    (incf-hash (elt polymer (- (length polymer) 1)) -1 count-hash)
    (let ((-max- (apply 'max (loop for vals being the hash-values in
                                   count-hash collect vals)))
          (-min- (apply 'min (loop for vals being the hash-values in
                                   count-hash collect vals))))
      (maphash (lambda (x y) (progn (print x) (print y))) count-hash)
      (values (/ (- -max- -min-) 2)))))

(let ((ans-a (part-a 10)))
  (incf ans-a)
  (if *testing* (assert (= ans-a 1588)))
  (print ans-a))
(let ((ans-b (part-a 30)))
  (incf ans-b)
  (if *testing* (assert (= ans-b 2188189693529)))
  (print ans-b))
