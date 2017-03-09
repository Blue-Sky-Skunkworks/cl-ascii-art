(in-package :cl-ascii-art)

(defvar *note-lock* (sb-thread:make-mutex))
(defparameter *inhibit-note* nil)

(defmacro with-note-lock (&body body) `(with-mutex (*note-lock*) ,@body))

(defparameter *note-start-clock* (let ((now (get-universal-time)))
                                   (format t "~&;;  Note logging started at: ~A.~%" now)
                                   now))

(defparameter *show-note-clock* nil)

(defun note (control &rest arguments)
  (unless *inhibit-note*
    (let ((*print-pretty* nil))
      (sb-thread:with-mutex (*note-lock*)
        (apply #'format t
               (format nil "~~&;; ~@[ ~A ~]~A ~A~~%"
                       (and *show-note-clock* (blue (princ-to-string (clock-face)) :effect :bright))
                       (blue (princ-to-string (- (get-universal-time) *note-start-clock*))
                             :effect :bright)
                       control) arguments)
        (finish-output t)))))

(defmacro bugout (&rest vars)
  "Print VARS, for debugging."
  (when-let ((len (iter (for var in vars)
                        (maximizing (length (prin1-to-string var))))))
    `(note ,(with-output-to-string (s)
                                   (write-string "BUGOUT  " s)
                                   (iter (for els on vars)
                                         (let ((var (car els)))
                                           (format s (format nil "~~~AS" (+ len 2)) var)
                                           (unless (keywordp var) (write-string " ==>  ~S" s))
                                           (when (cdr els) (format s "~%;;               ")))))
           ,@(remove-if #'keywordp vars))))

