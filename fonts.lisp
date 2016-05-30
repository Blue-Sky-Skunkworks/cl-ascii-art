(in-package :cl-ascii-art)

(defclass font ()
  ((name :initarg :name :reader name)
   (height :initarg :height :reader height)
   (baseline :initarg :baseline :reader baseline)
   (max-length :initarg :max-length :reader max-length)
   (old-layout :initarg :old-layout :reader old-layout)
   (common-lines :initarg :common-lines :reader common-lines)
   (print-direction :initarg :print-direction :reader print-direction)
   (full-layout :initarg :full-layout :reader full-layout)
   (code-count :initarg :code-count :reader code-count)))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t)
    (format stream "~A" (name font))))

(defun parse-font-file (path)
  (with-input-from-file (stream path)
    (when-let (line (handler-case
                        (string-trim '(#\return) (read-line stream))
                      (error (c) (warn "Error reading font file ~S ~A." path c)) ))
      (unless (string-starts-with line "flf2a")
        (error "Invalid font file ~S." path))
      (let ((hardblank (char line 5)))
        (destructuring-bind (height baseline max-length old-layout common-lines &optional print-direction full-layout code-count)
            (iter (for el in (split-sequence #\space line :start 6 :remove-empty-subseqs t))
              (for x from 1 to 8)
              (collect (parse-integer el)))
          (make-instance 'font :name (pathname-name path)
                               :height height :baseline baseline :max-length max-length
                               :old-layout old-layout :common-lines common-lines
                               :print-direction print-direction :full-layout full-layout
                               :code-count code-count))))))

(defun load-font-directory (&optional (path *font-directory*))
  (iter (for file in (directory-files path))
    (when (member (pathname-type file) '("flf") :test 'equal)
      (when-let ((font (parse-font-file file)))
        (collect font)))))

(defparameter *font-directory* (art-file "fonts/"))

(define-selection-menu fonts (font *fonts* *font* :default "standard" :reader name)
  (load-font-directory))

(defun find-font-from-name (name)
  (or (car (member name *fonts* :test 'equal :key 'name))
      (error "Unknown font ~S." name)))

(defun find-font (name-or-index)
  (typecase name-or-index
    (font name-or-index)
    (string (find-font-from-name name-or-index))
    (integer
     (if (or (< name-or-index 1) (> name-or-index (length *fonts*)))
         (error "Ascii font index out of range.")
         (nth (1- name-or-index) *fonts*)))))

(defun select-font (name-or-index)
  (setf *font* (find-font name-or-index)))

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

