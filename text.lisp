(in-package :cl-ascii-art)

(defparameter *font-directory* (art-file "fonts/"))

(defvar *fonts* nil)

(defun find-font-from-name (name)
  (if (member name *fonts* :test 'equal)
      name
      (error "Unknown font ~S." name)))

(defun load-fonts ()
  (setf *fonts*
        (sort
         (iter (for file in (directory-files *font-directory*))
           (let ((type (pathname-type file)))
             (when (member type '("flf" "tlf") :test 'string=)
               (collect (pathname-name file)))))
         #'string<)))

(load-fonts)

(defparameter *font* "standard")

(defun fonts ()
  (let ((index 0))
    (iter (for row in (group *fonts* 4))
      (iter (for name in row)
        (if (string= name *font*)
            (format t "~2D  ~22A  " (incf index) (cyan name :effect :bright))
            (format t "~2D  ~11A  " (incf index) name)))
      (terpri))))

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
    (run-program stream "toilet" (nconc
                                  (list "-f" *font* "-w" width "-d" *font-directory*)
                                  (when (plusp (length filter)) (list "-F" filter))
                                  (when full-width (list "-W"))
                                  (list (prin1-to-string text))))
    (values)))

(defun line-count (string)
  (count #\newline string))

(defun line-width (string)
  (position #\newline string))

(defun demo-fonts (&key count (text "AaBbCc123!@#") (width 120)
                     max-height min-height max-width min-width)
  (iter (for font in *fonts*)
    (for index from 1 to (or count most-positive-fixnum))
    (let* ((demo (with-output-to-string (*standard-output*)
                   (text text :font font :width width)))
           (count (line-count demo))
           (line-width (line-width demo)))
      (when (and (or (null max-height) (<= count max-height))
                 (or (null min-height) (>= count min-height))
                 (or (null max-width) (<= line-width max-width))
                 (or (null min-width) (>= line-width min-width)))
        (format t "~%~A~%~%~A" (white font) demo)))))

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
