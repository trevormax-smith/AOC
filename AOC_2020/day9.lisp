(defparameter *input9* (uiop:read-file-lines "~/Documents/Lisp/input9"))
(setf *input9* (map 'list (lambda (x)
             (parse-integer (string x))) *input9*))

(defparameter *amble* (subseq *input9* 0 25))

(defun update-amble (i) 
  (setf *amble* (subseq *input9* (+ 0 i) (+ 25 i))))

(defparameter i 0)

(defun check-item-is-sum (i)
  (let ((checklist
          (loop for i from 0 to 23
                append (loop for j from (+ i 1) to 24
                             collect (list (elt *amble* i) (elt *amble* j)))))
        (element (elt *input9* (+ i 25))))
    (delete-if (lambda (x) (= (elt x 0) (elt x 1))) checklist)
    (setf checklist (map 'list (lambda (x) (apply '+ x)) checklist))
    (if (find element checklist) (values nil) (values element))))

(defparameter *invalid-number*
  (dotimes (i (length *input9*))
    (update-amble i)
    (let ((r (check-item-is-sum i)))
      (if (not (eq r nil)) (return r)))))

(defun check-contigous-of-length (n)
  (loop for i from 0 to (- (length *input9*) n) do (solved-p (subseq *input9* i (+ i n)))))

(defun smallest-and-largest (l)
  (+ (apply 'min l) (apply 'max l)))

(defun solved-p (l)
  (if (= (apply '+ l) *invalid-number*) (print (smallest-and-largest l))))

(dotimes (n 1000)
    (check-contigous-of-length n))
