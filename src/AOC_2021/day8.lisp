(defparameter *testing*-a nil)
(defparameter *testing*-b nil)
(defparameter *input8* (list))

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day8_input"))
  (if *testing*-a (setf filename (concatenate 'string filename "_test")))
  (if *testing*-b (setf filename (concatenate 'string filename "_test_b")))
  (setf *input8* (uiop:read-file-lines filename)))

(defun parse-input (row)
  (map 'list (lambda (row-part) (str:split " " row-part))
       (str:split " | " row)))

(setf *input8* (map 'list 'parse-input *input8*))

#| yay |#
(defparameter length-possibility-map (make-hash-table))
(setf (gethash 2 length-possibility-map) '(1))
(setf (gethash 3 length-possibility-map) '(7))
(setf (gethash 4 length-possibility-map) '(4))
(setf (gethash 5 length-possibility-map) '(2 3 5))
(setf (gethash 6 length-possibility-map) '(0 6 9))
(setf (gethash 7 length-possibility-map) '(8))

(defparameter number-segment-map (make-hash-table))
(setf (gethash 0 number-segment-map) (coerce "abcefg" 'list))
(setf (gethash 1 number-segment-map) (coerce "cf" 'list))
(setf (gethash 2 number-segment-map) (coerce "acdeg" 'list))
(setf (gethash 3 number-segment-map) (coerce "acdfg" 'list))
(setf (gethash 4 number-segment-map) (coerce "bcdf" 'list))
(setf (gethash 5 number-segment-map) (coerce "abdfg" 'list))
(setf (gethash 6 number-segment-map) (coerce "abdefg" 'list))
(setf (gethash 7 number-segment-map) (coerce "acf" 'list))
(setf (gethash 8 number-segment-map) (coerce "abcdefg" 'list))
(setf (gethash 9 number-segment-map) (coerce "abcdfg" 'list))

(defparameter segment-number-map (make-hash-table :test 'equal))

(maphash
  (lambda (number-displayed segments)
    (setf (gethash segments segment-number-map) number-displayed)) number-segment-map)

(defun number-possibility (combination)
  (gethash (length combination) length-possibility-map))

(defun length-to-comb (row-part part-b)
  (loop for combination in row-part
        when (or part-b (= (length (number-possibility combination)) 1))
        collect (list (number-possibility combination)
                      (coerce combination 'list))))

(defun count-part-a (*input8*)
  (reduce '+ (loop for row in *input8* collect
                   (length (length-to-comb (elt row 1) nil)))))

(defparameter answer-a (count-part-a *input8*))
(if *testing*-a (assert (= answer-a 26)))

(print answer-a)

(defparameter *current-row-map* (make-hash-table :test 'equal))

(defun coerce-list (combo)
  (coerce combo 'list))
 
(defparameter *letters* (coerce "abcdefg" 'list))

(defmacro length-x-p (len)
  `(lambda (x) (= ,len (length x))))

(defun found-letters ()
  (loop for v being the hash-values in *current-row-map* collect v))

(defun letter-found (letter)
  (find letter (found-letters)))

(defun remove-found-letters (combinations)
  (map 'list (lambda (combo) (remove-if 'letter-found combo)) combinations))

(defun collect-length-x-combos (x row-combos)
  (loop for combo in row-combos when (= x (length combo)) collect combo))

(defun find-a (row-combos)
  (setf (gethash #\a *current-row-map*)
        (elt (set-exclusive-or
                 (coerce (elt (remove-if-not (length-x-p 2) row-combos) 0) 'list)
                 (coerce (elt (remove-if-not (length-x-p 3) row-combos) 0) 'list)) 0)))

(defun find-g (row-combos)
  (let ((four (elt (remove-if-not (length-x-p 4) row-combos) 0))
        (length-6-combos (remove-if-not (length-x-p 6) row-combos)))
    (loop for combo in length-6-combos
          do (let ((xor (set-exclusive-or
                          (concatenate 'list four (list (gethash #\a *current-row-map*)))
                          combo)))
               (if (funcall (length-x-p 1) xor)
                   (setf (gethash #\g *current-row-map*) (elt xor 0)))))))

(defun find-d (row-combos)
  (let ((length-5-combos (remove-found-letters (remove-if-not (length-x-p 5) row-combos))))
    (setf (gethash #\d *current-row-map*)
          (elt (intersection 
                 (intersection
                   (elt length-5-combos 0)
                   (elt length-5-combos 1))
                 (elt length-5-combos 2)) 0))))

(defun find-b (row-combos)
  (let ((one (elt (collect-length-x-combos 2 row-combos) 0))
        (four (elt (collect-length-x-combos 4 row-combos) 0)))
    (setf (gethash #\b *current-row-map*)
          (elt (set-difference
                 (set-difference four one)
                 (list (gethash #\d *current-row-map*))) 0))))

(defun find-f (row-combos)
  (loop for combo in
        (remove-found-letters (collect-length-x-combos 5 row-combos))
        when (= 1 (length combo)) do (setf (gethash #\f *current-row-map*) (elt combo 0))))

(defun find-c (row-combos)
  (setf (gethash #\c *current-row-map*) 
        (elt (set-difference (elt (collect-length-x-combos 2 row-combos) 0)
                             (list (gethash #\f *current-row-map*))) 0)))

(defun find-e (row-combos)
  (setf (gethash #\e *current-row-map*) 
        (elt (elt (remove-found-letters (collect-length-x-combos 7 row-combos)) 0) 0)))

(defun find-correct-mapping (row-combos)
  (find-a row-combos)
  (find-g row-combos)
  (find-d row-combos)
  (find-b row-combos)
  (find-f row-combos) 
  (find-c row-combos)
  (find-e row-combos))

(defun print-hash (hash)
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) hash))

(defun coerce-to-list (combos)
  (map 'list (lambda (combo) (coerce combo 'list)) combos))

(defun correct-combo (combo wrong-right-map)
  (loop for segment in combo collect (gethash segment wrong-right-map)))

(defun list-ints-to-int (listints)
  (loop for powten downfrom 3 to 0
        for int in listints
        sum (* (expt 10 powten) int) into sum
        finally (return sum)))

(defun solve-row (row-parts)
  (setf *current-row-map* (make-hash-table :test 'equal))
  (find-correct-mapping (coerce-to-list (elt row-parts 0)))
  (let ((wrong-right (make-hash-table :test 'equal)))
    (maphash
      (lambda (right wrong)
        (setf (gethash wrong wrong-right) right)) *current-row-map*)
    (return-from solve-row
      (list-ints-to-int 
        (loop for readout in (coerce-to-list (elt row-parts 1))
              collect (gethash
                        (sort (correct-combo readout wrong-right) 'char<)
                        segment-number-map))))))

(defun part-b ()
  (reduce '+ (map 'list 'solve-row *input8*)))

(if *testing*-a (assert (= (part-b) 61229)))

(part-b)
