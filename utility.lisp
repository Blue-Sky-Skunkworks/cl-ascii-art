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

(defun run-program-to-string (program args)
  (with-output-to-string (str)
    (asdf/run-program:run-program (format nil "~A ~{~A~^ ~}" program args) :output str)))

