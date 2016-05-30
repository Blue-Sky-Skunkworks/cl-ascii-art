(in-package :cl-ascii-art)

(defun  load-font-directory (&optional (path *font-directory*))
  (directory-filenames path '("flf" "tlf")))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t)
    (format stream "~A" (name font))))

(defparameter *font-directory* (art-file "fonts/"))

(define-selection-menu fonts (font *fonts* *font* :default "standard")
  (load-font-directory))

(defun find-font-from-name (name)
  (if (member name *fonts* :test 'equal)
      name
      (error "Unknown font ~S." name)))

(defun find-font (name-or-index)
  (typecase name-or-index
    (string (find-font-from-name name-or-index))
    (integer
     (if (or (< name-or-index 1) (> name-or-index (length *fonts*)))
         (error "Ascii font index out of range.")
         (nth (1- name-or-index) *fonts*)))))

(defun select-font (name-or-index)
  (setf *font* (find-font name-or-index)))

(defun find-font-from-name (name)
  (if (member name *fonts* :test 'equal)
      name
      (error "Unknown font ~S." name)))

(defvar *font-case* nil)

(defun font-case (name)
  (unless *font-case* (setf *font-case* (make-hash-table :test 'equal)))
  (gethash-set name *font-case*
    (let* ((font (find-font-from-name name))
           (upper (as-string (text "A" :font font)))
           (lower (as-string (text "a" :font font))))
      (not (string= upper lower)))))

(defun set-font-cases ()
  (iter (for font in *fonts*) (font-case font)))

