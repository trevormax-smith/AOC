(defparameter *testing* nil)
(defparameter *input16* nil)
(ql:quickload "array-operations")
(ql:quickload "select")
(ql:quickload "fset")

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day16_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input16* (uiop:read-file-lines filename)))
  (setf *input16* (elt *input16* 0)) 

(defun hex-char-to-num (-character-)
  (if (digit-char-p -character-) (digit-char-p -character-)
      (elt '(10 11 12 13 14 15) (position -character- "ABCDEF"))))

(defun num-to-bits (num)
  (let ((bvector (make-array 4 :element-type 'bit)))
    (loop for digit in '(8 4 2 1)
          for place in '(0 1 2 3)
          when (<= digit num) do (setf (aref bvector place) 1)
          when (<= digit num) do (decf num digit))
    (values bvector)))

(defun bits-to-num (bvector)
  (let ((index (make-array
                 (array-dimension bvector 0)
                 :initial-contents (loop for i downfrom (- (length bvector) 1)
                                         to 0 collect i))))
    (let ((vals (aops:vectorize (bvector index) (* bvector (expt 2 index)))))
      (aops:sum-index i (aref vals i)))))

(defun reshape-1 (vect)
  (aops:reshape (aops:combine vect) '(t)))

(setf *input16* (reshape-1 (aops:vectorize (*input16*)
                             (num-to-bits (hex-char-to-num *input16*)))))

(defparameter version-sum 0)

(defun literal-value (literal)
  (let ((num-vectors '(0))
        (newvec '()))
    (loop for i upfrom 0
          do (setf newvec 
                   (select:select literal
                                  (select:including 0 4)))
          do (setf literal
                   (if (<= 5 (length literal))
                       (select:select literal (select:including 5 -1))
                       '()))
          do (push (select:select newvec (select:including 1 -1)) num-vectors)
          when (= 0 (aref newvec 0)) do
          (return literal))
    (return-from
      literal-value
      (list (bits-to-num (reshape-1 (apply 'aops:stack (reverse num-vectors))))
            (+ 6 (* 5 (+ -1 (length num-vectors))))
            literal))))

(defun stop-logic (type-ID packet-length num-packets stop-cond-val)
  (ecase type-ID
    (0 (values (>= packet-length stop-cond-val)))
    (1 (values (>= num-packets stop-cond-val)))))

(defun make-reducer (func)
  (lambda (x) (reduce func x)))
(defun make-comparer (func)
  (lambda (x) (if (funcall func (elt x 0) (elt x 1)) 1 0)))

(defparameter *f* (fset:map
    (0 (make-reducer '+))
    (1 (make-reducer '*))
    (2 (make-reducer 'min))
    (3 (make-reducer 'max))
    (5 (make-comparer '>))
    (6 (make-comparer '<))
    (7 (make-comparer '=))))

(defun parse-packet-return-remainder (packet)
  (incf version-sum (bits-to-num (select:select packet '(0 1 2))))
  (case (bits-to-num (select:select packet '(3 4 5)))
    (4 (return-from parse-packet-return-remainder
                    (literal-value (select:select packet (select:including 6 -1)))))
    (otherwise
      (loop
        ; "doing alotta setup" 
        with type-ID
        = (select:select packet 6)
        with stop-cond-len
        = (if (= 0 (select:select packet 6)) 15 11)
        with first-packet-start
        = (+ 7 stop-cond-len)
        with stop-cond-val
        = (bits-to-num
            (select:select packet (select:range 7 first-packet-start)))
        ; "Actually parsing any inner packets"
        with subpacket
        = (select:select packet (select:including first-packet-start -1))
        with opfunc
        = (fset:lookup *f* (bits-to-num (select:select packet '(3 4 5))))
        with debug = (print opfunc)
        for i from 1 below 10000
        for recurve-return = (parse-packet-return-remainder subpacket)
        for packet-value = (elt recurve-return 0)
        for packet-length = (elt recurve-return 1)
        do (setf subpacket (elt recurve-return 2))
        collect packet-value into this-packet-value
        sum packet-length into this-packet-length
        when (stop-logic type-ID this-packet-length i stop-cond-val)
        do (progn
             (return-from parse-packet-return-remainder
                          (list (funcall opfunc this-packet-value)
                                (+ first-packet-start this-packet-length)
                                subpacket)))))))

(defparameter results (elt (parse-packet-return-remainder *input16*) 0))

(print version-sum)
(print results)
