(defparameter *input7* (uiop:read-file-lines "~/Documents/Lisp/input7"))

(defparameter *bag-hash* (make-hash-table))

(defun parse-bags (rule)
  (let ((outer-inner (str:split " contain " rule)))
    (list (elt outer-inner 0) (str:split ", " (elt outer-inner 1)))))

(defun clean-bags. (item)
  (string-right-trim " bags." item))

(defun convert-outer (outer)
  (read-from-string (substitute #\- #\ (clean-bags. outer))))
 
(defun add-rule (item)
  (setf (gethash (convert-outer (elt item 0)) *bag-hash*)
        (map 'list 'clean-bags. (elt item 1))))

(mapc (lambda (rule) (add-rule (parse-bags rule))) *input7*)

(defun clean-entries (inners)
  (map 'list (lambda (inner) (convert-outer (subseq inner 2))) inners))

(defun bag-in-rulep (rule bag)
  (loop for inner in (clean-entries rule)
    when (eq inner bag)
    return t))

(defun find-outers (bag)
  (loop for outer being the hash-keys of *bag-hash*
        if (bag-in-rulep (gethash outer *bag-hash*) bag)
        collect outer))

(defparameter *outers* (list))

(defun check-bags (bags)
  (dolist (bag bags)
    (let ((outers (find-outers bag)))
      (setf *outers* (append *outers* outers))
      (check-bags outers))))

(check-bags (list 'shiny-gold))

(length (remove-duplicates *outers*))

(defparameter *inner-total* 0)

(defun bag-num (inner)
  (digit-char-p (elt inner 0)))

(defun check-contains (bag num)
  (let ((outers (gethash bag *bag-hash*)))
    (dolist (outer outers)
      (if (not (search "no other" outer))
          (list (incf *inner-total* (* (bag-num outer) num))
                (check-contains 
                  (convert-outer (subseq outer 2))
                  (* (bag-num outer) num)))))))

(check-contains 'shiny-gold 1)
