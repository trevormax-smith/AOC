(ql:quickload "uiop")
(ql:quickload "split-sequence")

(defparameter *testing* nil)
(defparameter *input* (list))
(defparameter *steps* (list))
(defparameter *stacks* (list))

(let ((filename "~/Documents/Lisp/AOC/src/AOC_2022/inputs/5"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (loop
    with stacks = t
    with *
    for line in (uiop:read-file-lines filename)
    when (string= line "") do (setf stacks nil)
    when (equal stacks t) do (push line *stacks*)
    when (equal stacks nil) do (push line *steps*))
  (let ((stacknums (pop *stacks*)))
    (setf *steps* (reverse *steps*))
    (pop *steps*)
    (loop for i in
          (remove "" (split-sequence:split-sequence #\space stacknums) :test 'string=)
          do (push (list) *input*))
    (loop for stack in *stacks* do
          (loop for i from 0 below (length *input*)
                when (not (eql (elt stack (+ (* i 4) 1)) #\space))
                do (push (elt stack (+ (* i 4) 1)) (elt *input* i))))))

(defparameter *input2* (copy-tree *input*))

(defun read-step (the-step)
  (let ((pstep (split-sequence:split-sequence #\space the-step)))
    (mapcar 'read-from-string
            (remove "from"
                    (remove "to"
                            (remove "move" pstep :test 'string=)
                            :test 'string=)
                    :test 'string=))))

(defun move (n from to)
  (decf from)
  (decf to)
  (dotimes (i n) (push (pop (elt *input* from)) (elt *input* to))))

(defun part-a ()
  (mapcar (lambda (x) (apply 'move (read-step x))) *steps*)
  (coerce (mapcar 'car *input*) 'string))

(defun move-2 (n from to)
  (decf from)
  (decf to)
  (setf (elt *input2* to) (append (subseq (elt *input2* from) 0 n) (elt *input2* to)))
  (setf (elt *input2* from) (subseq (elt *input2* from) n)))

(defun part-b ()
  (mapcar (lambda (x) (apply 'move-2 (read-step x))) *steps*)
  (coerce (mapcar 'car *input2*) 'string))

(let ((a-ans (part-a))
      (b-ans (part-b)))
  (if *testing*
      (assert (equal "CMZ" a-ans))
      (print a-ans))
  (if *testing*
      (assert (equal "MCD" b-ans))
      (print b-ans)))

