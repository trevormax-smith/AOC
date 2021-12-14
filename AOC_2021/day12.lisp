(load "~/quicklisp/setup.lisp")
(ql:quickload "str")
(ql:quickload "alexandria")

(defparameter *testing* t)
(defparameter *input12* (list))

(let ((filename "~/Documents/Lisp/AOC/AOC_2021/day12_input"))
  (if *testing* (setf filename (concatenate 'string filename "_test")))
  (setf *input12* (uiop:read-file-lines filename)))

(setf *input12* (map 'list (lambda (path) (str:split "-" path)) *input12*)) 

(defparameter cave-hash (make-hash-table :test 'equal))

(defun add-path (path)
  (push (elt path 1) (gethash (elt path 0) cave-hash (list)))
  (push (elt path 0) (gethash (elt path 1) cave-hash (list))))

(defun print-hash (hash)
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) hash))

(mapc 'add-path *input12*)

(defun reset-cave-hash ()
  (setf cave-hash (make-hash-table :test 'equal))
  (mapc 'add-path *input12*))

(defparameter lowercase-visited '())
(defparameter visited-twice '())

(defun remove-small-from-all (item)
  (loop for node being the hash-keys in cave-hash do
        (remove-from-node item node)))

(defun remove-from-node (item node)
  (setf (gethash node cave-hash)
        (remove item (gethash node cave-hash) :test #'equal)))

(defun uppercase-p (st) (not (string= (string-upcase st) st)))

(defun take-step (from to current-path part-b)
  (cond (part-b
          (cond ((string= from "start")
                 (remove-small-from-all from))
                ((and (not (uppercase-p from)) visited-twice)
                 (remove-small-from-all from))
                ((find to lowercase-visited :test #'equal)
                 (list (mapc 'remove-small-from-all lowercase-visited)
                       (setf visited-twice t)
                       (remove-small-from-all to)))
                ((and (not (string= (string-upcase from) from)) (not visited-twice))
                 (push from lowercase-visited))))
        ((not part-b)
         (if (not (string= (string-upcase from) from))
             (remove-small-from-all from))))
  (append current-path (list to)))

(defparameter taken-full-paths (list))

(defun eval-paths-return-next-or-nil (from current-path)
  (dolist (next-step (gethash from cave-hash))
    (if (not (find (append current-path
                           (list next-step))
                   taken-full-paths :test #'equal))
        (return-from eval-paths-return-next-or-nil next-step)))
  (return-from eval-paths-return-next-or-nil nil))

(defun build-path (from current-path part-b)
  (let ((next-step (eval-paths-return-next-or-nil from current-path)))
    (cond ((string= next-step "end")
           (push (append current-path '("end")) taken-full-paths))
          ((not next-step)
           (push current-path taken-full-paths))
          (next-step
           (list
             (setf current-path (take-step from next-step current-path part-b))
             (build-path next-step current-path part-b))))))

(defun valid-path (path) (equal '("end") (last path)))

(defun all-valid-paths (part-b)
  (do ()
      ((not (eval-paths-return-next-or-nil "start" '("start"))))
      (build-path "start" '("start") part-b)
      (reset-cave-hash)
      (setf visited-twice nil)
      (setf lowercase-visited '()))
  (mapc (lambda (p) (format t "~a~%" p)) taken-full-paths)
  (setf taken-full-paths (remove-if-not 'valid-path taken-full-paths))
  (length taken-full-paths))

(let ((answer-a (all-valid-paths nil)))
  (if *testing* (assert (= answer-a 19)))
  (print answer-a))
(let ((answer-b (all-valid-paths t)))
  (if *testing* (assert (= answer-b 103)))
  (print answer-b))
