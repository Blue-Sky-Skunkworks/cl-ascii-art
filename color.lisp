(in-package :cl-ascii-art)

(defmacro if-color (test color &body body)
  (let ((var (gensym))
        (rtn (gensym)))
    `(let ((,rtn (progn ,@body)))
       (if ,test
           (with-output-to-string (stream)
             (with-color ,(append (ensure-list color)
                           (list :stream 'stream))
               (princ ,rtn stream)))
           ,rtn))))

(defun length-mono (string)
  "Returns the length of a string minus the ANSI control sequences."
  (let ((count 0))
    (iter (with inside)
      (for c in-vector string)
      (cond
        (inside (when (char= c #\m) (setf inside nil)))
        ((char= c #\esc) (setf inside t))
        (t (incf count))))
    count))

(defun control-length (string)
  "Returns the length of the ANSI control sequences in STRING."
  (let ((count 0))
    (iter (with inside)
      (for c in-vector string)
      (cond
        (inside (when (char= c #\m) (setf inside nil)) (incf count))
        ((char= c #\esc) (setf inside t) (incf count))))
    count))
