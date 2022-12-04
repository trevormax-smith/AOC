(defun load-clean ()
  (defparameter *input8* (uiop:read-file-lines "~/Documents/Lisp/input8"))
  (setf *input8* (map 'list (lambda (instr) (list 0 instr)) *input8*)))

(load-clean)

(defparameter *accumulator* 0)
(defparameter *parser* 0)

(defun acc (num)
  (list (incf *accumulator* num) (incf *parser* 1)))

(defun nop (num)
  (incf *parser* 1))

(defun jmp (num)
  (incf *parser* num))

(defun call-string (instruction)
  (funcall
     (read-from-string (subseq instruction 0 3))
     (parse-integer (subseq instruction 4))))

(defun access-instruction (index)
  (if (eq index (length *INPUT8*))
      (values t)
      (let ((instruction (elt *input8* index)))
        (if 
            (= (incf (elt instruction 0)) 2)
            (values t)
            (list 
              (call-string (elt instruction 1))
              (access-instruction *parser*)))
        (values nil))))

(defun swap-nop-jmp (instruction)
  (cond 
    ((string= (subseq instruction 0 3) "jmp") (setf (subseq instruction 0 3) "nop"))
    ((string= (subseq instruction 0 3) "nop") (setf (subseq instruction 0 3) "jmp"))
    ((string= (subseq instruction 0 3) "acc") (values))))

(defun try-fix (index)
  (swap-nop-jmp (elt (elt *input8* index) 1))
  (if (not (access-instruction 0))
      (elt 
        (list 
          (swap-nop-jmp (elt (elt *input8* index) 1))
          (setf *accumulator* 0)
          (setf *parser* 0)
          (load-clean)) 2) 
      (values *accumulator* "We did it boys")))

(dotimes (i (length *input8*))
  (print (list (try-fix i) i)))
