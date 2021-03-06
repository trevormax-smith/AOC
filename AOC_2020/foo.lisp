(defvar *db* nil)

(defun make-cd (title artist rating ripped)
   (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another?")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print  *db* out))))

(defun load-puzzle (filename)
  (let 
  (with-open-file (in filename
                       :direction :input)
    
    )))



(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title    (setf (getf row :title) title))
                (if artist   (setf (getf row :artist) artist))
                (if rating   (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped) ripped)))
              row) *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
   
(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(defun mod-square-9 (n)
  (mod (* n n) 9))


(defun make-count () 
  (let ((count 0))
    (list
      #'(lambda () (format t "~d " (incf count)))
      #'(lambda () (format t "~d " (decf count)))
      #'(lambda () (format t "~d " (count))))))
