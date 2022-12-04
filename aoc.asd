(defclass auto-module (module)
  ((file-cache :initform (make-hash-table))))

(defmethod component-children ((self auto-module)
                               &aux (file-cache (slot-value self 'file-cache)))
  (mapcar (lambda (p &aux (existing (gethash p file-cache)))
            (if existing
                existing
                (setf (gethash p file-cache)
                      (make-instance 'cl-source-file :type "lisp"
                                     :pathname p
                                     :name (pathname-name p)
                                     :parent (component-parent self)))))
          (directory-files (component-pathname self)
                           (make-pathname :directory nil :name *wild* :type "lisp"))))

(asdf:defsystem #:aoc
  :description "Advent of Code solutions in Common Lisp"
  :author "Trevor Smith"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:split-sequence)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "utils")
     (:auto-module "AOC_2022")))))
