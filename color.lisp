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

