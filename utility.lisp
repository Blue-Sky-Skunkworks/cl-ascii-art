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

(defun directory-filenames (path types)
  (sort
   (iter (for file in (directory-files path))
     (let ((type (pathname-type file)))
       (when (member type types :test 'string=)
         (collect (pathname-name file)))))
   #'string<))

(defun slurp-file (filename &optional external-format)
  (with-input-from-file (stream filename :external-format (or external-format :utf-8))
    (let* ((len (file-length stream))
           (seq (make-string len))
           (actual-len (read-sequence seq stream)))
      (if (< actual-len len)
        ;; KLUDGE eh, FILE-LENGTH doesn't know about utf8 so we use some duct tape
        (string-right-trim '(#\nul) seq)
        seq))))

(defun collect-scans (regex string)
  (let (acc)
    (do-scans (ms me rs re (create-scanner regex) string)
      (push (string-trim '(#\space) (subseq string (aref rs 0) (aref re 0))) acc))
    (nreverse acc)))

(defun make-list-generator (list)
  (lambda ()
    (values (car list)
            (progn
              (setf list (cdr list))
              (not (null list))))))

(defun make-list-fn-generator (list fn)
  (lambda ()
    (values (funcall fn (car list))
            (progn
              (setf list (cdr list))
              (not (null list))))))

(defmacro gethash-set (key hash-table &body body)
  (let ((%key (gensym))
        (%hash (gensym)))
    `(let ((,%key ,key)
           (,%hash ,hash-table))
       (multiple-value-bind (val hit) (gethash ,%key ,%hash)
         (if hit
             val
             (setf (gethash ,%key ,%hash) (progn ,@body)))))))

(defun string-starts-with (string prefix &key (test #'char=))
  "Returns true if STRING starts with PREFIX."
  (let ((mismatch (mismatch prefix string :test test)))
    (or (not mismatch) (= mismatch (length prefix)))))

