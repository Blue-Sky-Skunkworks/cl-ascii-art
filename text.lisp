(in-package :cl-ascii-art)

(defparameter *font-directory* (art-file "fonts/"))
(define-selection-menu fonts font *fonts* *font* "standard"
  (directory-filenames *font-directory* '("flf" "tlf")))

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

(defun text (text &key (stream *standard-output*) (font *font*) (width 80)
                    border crop gay metal left right full-width)
  (let ((*font* (find-font font))
        (filter (format nil "~{~@[~A~^:~]~}" (list (and border "border")
                                                   (and crop "crop")
                                                   (and gay "gay")
                                                   (and metal "metal")
                                                   (and left "left")
                                                   (and right "right")))))
    (run `(toilet "-f" ,*font* "-w" ,width "-d" ,*font-directory*
                  ,@(when (plusp (length filter)) `("-F" ,filter))
                  ,@(when full-width (list "-W"))
                  ,(princ-to-string text)) :output stream)
    (values)))

(defun line-count (string)
  (count #\newline string))

(defun line-width (string)
  (position #\newline string))

(defun indent-text (text count &key skip-first (stream *standard-output*) (char #\space))
  (let ((lines (split-sequence #\newline text))
        (indent (make-string count :initial-element char)))
    (iter (for els on lines)
      (unless (and skip-first (first-iteration-p))
        (princ indent stream))
      (princ (car els) stream)
      (when (cdr els) (terpri stream)))))

;;; from https://www.rosettacode.org/wiki/Word_wrap#Common_Lisp

(defun word-wrap (str width)
  (setq str (concatenate 'string str " ")) ; add sentinel
  (with-output-to-string (stream)
    (do* ((len (length str))
          (begin-curr-line 0)
          (prev-space 0 pos-space)
          (pos-space (position #\Space str) (when (< (1+ prev-space) len) (position #\Space str :start (1+ prev-space)))))
         ((null pos-space) (format stream "~A~%" (subseq str begin-curr-line (1- len))))
      (when (> (- pos-space begin-curr-line) width)
        (format stream "~A~%" (subseq str begin-curr-line prev-space))
        (setq begin-curr-line (1+ prev-space))))))

(defun unfill-paragraph (text)
  (delete #\newline text))

(defun heading (string &key (char #\=) (stream *standard-output*))
  (format stream "~%~A~%" string)
  (dotimes (x (length string)) (write-char char stream))
  (format stream "~%~%"))

(defun print-with-ellipses (el &key (stream t) (max 20) (fn #'princ))
  (let* ((string (princ-to-string el))
         (len (length string)))
    (cond
      ((or (null max) (< len max)) (funcall fn string stream))
      (t (funcall fn (subseq string 0 max) stream)
         (funcall fn #\horizontal_ellipsis stream))))
  el)

(defun remove-trailing-newline (string)
  (let ((pos (1- (length string))))
    (if (char= (char string pos) #\newline)
        (subseq string 0 pos)
        string)))
