(in-package :cl-ascii-art)

(defun group (list n)
  "Group the elements of LIST into lists of N elements each."
  (when (zerop n) (error "Groups of zero are no fun."))
  (labels ((rec (list acc)
             (let ((rest (nthcdr n list)))
               (if (consp rest)
                 (rec rest (cons
                            (subseq list 0 n)
                            acc))
                 (nreverse
                  (cons list acc))))))
    (when list (rec list nil))))

(defun run-program-on-data (program args data stream)
  (let ((proc
          (let* ((proc (sb-ext:run-program program args
                                           :input :stream
                                           :output stream
                                           :search t
                                           :wait nil))
                 (in (sb-ext:process-input proc)))
            (write-sequence data in)
            (close in)
            (sb-ext:process-wait proc))))
    (unless (and (eq :exited (sb-ext:process-status proc))
                 (zerop (sb-ext:process-exit-code proc)))
      (cerror "Return whatever output was gotten." "Running ~A failed." program))))

(defun run-program-to-string (program args)
  (with-output-to-string (str)
    (asdf/run-program:run-program (format nil "~A ~{~A~^ ~}" program args) :output str)))

(defun run-program (stream program args)
  (asdf/run-program:run-program (format nil "~A ~{~A~^ ~}" program args) :output stream))

(defun asdf-base-path (name)
  (directory-namestring (asdf:component-pathname (asdf:find-system name))))

(defun art-file (&optional  base)
  (concatenate 'string (asdf-base-path :cl-ascii-art) base))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (when a (princ a s)))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;;; Adapted from cl-dsl. Props out to Alexander Popolitov
;;; <popolit@gmail.com> for unleasing a wave of easily written DLS's
;;; onto my world with such a simple set of macros.

(defun with-macrolets (would-be-macrolets body)
  `(macrolet ,(iter (for el in would-be-macrolets)
                (collect `(,(symb el) (&rest args) `(,',el ,@args))))
     ,body))

(defun with-flets (would-be-flets body)
  `(flet ,(iter (for el in would-be-flets)
            (collect `(,(symb el) (&rest args) (apply ',el args))))
     ,body))

(defmacro as-string (&body body)
  `(with-output-to-string (*standard-output*) ,@body))
