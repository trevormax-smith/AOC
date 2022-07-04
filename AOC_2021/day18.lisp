(defparameter *testing* nil)
(defparameter *input18* nil)

(defun convert-input (inp)
  (read-from-string
    (substitute #\space #\,
        (substitute #\) #\]
            (substitute #\( #\[ inp)))))

(defun load-input ()
  (let ((filename "~/Documents/Lisp/AOC/AOC_2021/day18_input"))
    (if *testing* (setf filename (concatenate 'string filename "_test")))
    (setf *input18* (uiop:read-file-lines filename))
    (setf *input18* (map 'list 'convert-input *input18*))))

(defun get-input ()
  (let ((filename "~/Documents/Lisp/AOC/AOC_2021/day18_input"))
    (if *testing* (setf filename (concatenate 'string filename "_test")))
    (setf input (uiop:read-file-lines filename))
    (setf input (map 'list 'convert-input input))))

(defparameter t1 '(((((9 8) 1) 2) 3) 4))
(defparameter t2 (convert-input "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))
(defparameter t3 '(0 (0 (0 ((1 2) (5 9))))))
(defparameter t4 (convert-input "[7,[6,[5,[4,[3,2]]]]]"))
(defparameter t5 '((((0 7) 4) ((7 8) (6 (6 7)))) (1 1)))
(defparameter t6 '((((7 0) (7 8)) ((7 9) (0 6))) (((7 0) 12) (14 (0 9)))))
(defparameter t7 (convert-input "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"))
(defparameter t8 (convert-input "[7,[5,[[3,8],[1,4]]]]"))
(defparameter t9 '((((7 7) (7 8)) ((9 5) (8 0))) (((9 10) 20) (8 (9 0)))))
 
(if *testing*
    (setf *input17* '((20 30) (-10 -5)))
    (setf *input17* '((32 65) (-225 -177))))

(defun icarp (l)
  (integerp (car l)))

(defun icadrp (l)
  (integerp (cadr l)))

(defun pair-p (snumber)
  (and (icarp snumber)
       (icadrp snumber)))

(defun split (rnumber)
  (let ((mval (/ rnumber 2)))
    (list (floor mval)
          (ceiling mval))))

(defun str-append (s1 s2)
  (concatenate 'string s1 s2))

(defun leftnext (smap)
  (loop for i downfrom (- (length smap) 1) to 0
        when (char-equal #\R (elt smap i))
          return (str-append (subseq smap 0 i) "L")))

(defun rightnext (smap)
  (loop for i downfrom (- (length smap) 1) to 0
        when (char-equal #\L (elt smap i))
          return (str-append (subseq smap 0 i) "R")))

(defun carmap (snumber smap)
  (cond
    ((= 0 (length smap)) snumber)
    ((and (integerp snumber) (length smap)) nil)
    (t (case (elt smap 0)
        (#\L (carmap (car snumber) (subseq smap 1)))
        (#\R (carmap (cadr snumber) (subseq smap 1)))))))

(defun left-first-search (snumber smap)
  (if (integerp snumber) (subseq smap 0 (- (length smap) 1))
      (if (icarp snumber) smap
          (left-first-search (car snumber) (str-append smap "L")))))

(defun right-first-search (snumber smap)
  (if (integerp snumber) (subseq smap 0 (- (length smap) 1))
      (if (icadrp snumber) smap
          (right-first-search (cadr snumber) (str-append smap "R")))))

(defun explode-right (snumber rnumber smap)
  (let ((rnext (rightnext smap)))
    (cond
      ((not rnext) nil)
      ((integerp (carmap snumber rnext))
               (incf (cadr (carmap snumber (subseq rnext 0 (- (length rnext) 1)))) rnumber))
      (t
               (incf (car (carmap snumber
                                  (left-first-search (carmap snumber rnext) rnext)))
                     rnumber)))))

(defun explode-left (snumber rnumber smap)
  (let ((lnext (leftnext smap)))
    (if lnext
        (if (integerp (carmap snumber lnext))
            (incf (car (carmap snumber (subseq lnext 0 (- (length lnext) 1)))) rnumber)
            (progn
              (incf (cadr (carmap snumber
                                  (right-first-search (carmap snumber lnext) lnext)))
                    rnumber))))))

(defun explode (snumber smap)
  (let ((left (car (carmap snumber smap)))
        (right (cadr (carmap snumber smap)))
        (higher (carmap snumber (subseq smap 0 (- (length smap) 1))))
        (lastdir (elt smap (- (length smap) 1))))
    (if (char= #\L lastdir)
        (setf (car higher) 0)
        (setf (cadr higher) 0))
    (explode-left snumber left smap)
    (explode-right snumber right smap)
    (values t)))

(defun find-explode (snumber smap)
  (let ((maybe-pair (carmap snumber smap)))
    (cond
      ((not smap) nil)
      ((integerp maybe-pair)
               (find-explode snumber (rightnext smap)))
      ((and (pair-p maybe-pair) (<= 4 (length smap)))
               (progn (explode snumber smap)
                      (find-explode snumber "")))
      ((and (pair-p maybe-pair) (> 4 (length smap)))
               (let ((rnext (rightnext smap)))
                 (if (not rnext) nil
                     (find-explode snumber rnext))))
      (t
               (find-explode snumber (str-append smap "L"))))))

(defun find-split (snumber smap)
  (let ((maybe-pair (carmap snumber smap)))
    (cond
      ((not smap) nil)
      ((and (integerp maybe-pair) (< 9 maybe-pair)) 
               (let ((last-dir (elt smap (- (length smap) 1)))
                     (up-pair (carmap snumber (subseq smap 0 (- (length smap) 1)))))
                 (case last-dir
                   (#\L (setf (car up-pair) (split maybe-pair)))
                   (#\R (setf (cadr up-pair) (split maybe-pair)))))
               t)
      ((integerp maybe-pair) (find-split snumber (rightnext smap)))
      ((listp maybe-pair)
               (find-split snumber (str-append smap "L"))))))


(defun sreduce (snumber)
  (find-explode snumber "")
  (if (find-split snumber (left-first-search snumber ""))
      (sreduce snumber)
      snumber))

(defun sadd (left-snumber right-snumber)
  (let ((snumber (list left-snumber right-snumber)))
    (sreduce snumber)))

(defun big-sadd (&rest snumbers)
  (let ((snumber (sadd (pop snumbers) (pop snumbers))))
    (loop for i upfrom 0 to (- (length snumbers) 1)
          do
          (print snumber)
          (setf snumber (sadd snumber (pop snumbers))))
    (print snumber)))

(defun smagnitude (snumber)
  (if (integerp snumber) snumber
      (+ (* 3 (smagnitude (car snumber)))
         (* 2 (smagnitude (cadr snumber))))))

(defun solve-a ()
  (load-input)
  (smagnitude (apply 'big-sadd *input18*)))

(defun solve-b ()
  (loop for i upfrom 0 below (length *input18*)
        maximizing (loop for j upfrom 0 below (length *input18*)
                         maximizing (smagnitude (sadd (elt (get-input) i)
                                                      (elt (get-input) j)))
                         into maxmag finally (return maxmag))
        into maxmag finally (return maxmag)))

(let ((ans-a (solve-a)))
  (if *testing* (assert (= ans-a 4140)))
  (print ans-a))

(let ((ans-b (solve-b)))
  (if *testing* (assert (= ans-b 3993)))
  (print ans-b))
