(defparameter *input12* (uiop:read-file-lines "~/Documents/Lisp/input12"))

(defparameter *facing* 0)
(defparameter *position* '(0 0))

(defmacro make-move-direction (var)
  `(lambda (angle distance)
     (case angle
       (0 (incf (elt ,var 0) distance))
       (90 (decf (elt ,var 1) distance))
       (180 (decf (elt ,var 0) distance))
       (270 (incf (elt ,var 1) distance)))))

(defun update-angle (angle)
  (incf *facing* angle)
  (setf *facing* (mod *facing* 360)))

(defun default-forward-mover (value)
  (move-direction *facing* value))

(defun parse-action (action angle-updater direction-mover forward-mover)
  (let ((code (elt action 0)) (value (parse-integer (subseq action 1))))
    (case code
      (#\R (funcall angle-updater value))
      (#\L (funcall angle-updater (* -1 value)))
      (#\F (funcall forward-mover value))
      (#\E (funcall direction-mover 0 value))
      (#\S (funcall direction-mover 90 value))
      (#\W (funcall direction-mover 180 value))
      (#\N (funcall direction-mover 270 value)))))

(defun rotate-waypoint (direction)
  (cond
    ((< direction 0)
     (dotimes (i (/ (abs direction)90))
       (setf *waypoint* (lla:mm #2A ((0 -1) (1 0)) *waypoint*))))
    ((> direction 0)
     (dotimes (i (/ direction  90))
       (setf *waypoint* (lla:mm #2A ((0 1) (-1 0)) *waypoint*))))))

(defun move-toward-waypoint (times)
  (dotimes (i times)
    (setf *position* (aops:each '+ *position* *waypoint*))))

(dolist (action *input12*) 
  (parse-action action 'update-angle (make-move-direction *position*) 'default-forward-mover))

(print (apply '+ (map 'list 'abs *position*)))

(defparameter *waypoint* #(10 1))
(setf *position* #(0 0))

(dolist (action *input12*) 
  (parse-action action 'rotate-waypoint (make-move-direction *waypoint*) 'move-toward-waypoint))

(apply '+ (map 'list 'abs *position*))
