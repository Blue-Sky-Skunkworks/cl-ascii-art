(in-package :cl-ascii-art)

(defparameter *font-directory* "/usr/share/figlet/")
(defvar *fonts* nil)

(defun find-font (name)
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

(defun select-font (name-or-index)
  (setf *font*
        (typecase name-or-index
          (string (find-font name-or-index))
          (integer
           (if (or (< name-or-index 1) (> name-or-index (length *fonts*)))
               (error "Ascii font index out of range.")
               (nth (1- name-or-index) *fonts*))))))

(defun text (text &key (font *font*) (width 80) border crop gay metal left right)
  (select-font font)
  (let ((filter (format nil "~{~@[~A~^:~]~}" (list (and border "border")
                                                   (and crop "crop")
                                                   (and gay "gay")
                                                   (and metal "metal")
                                                   (and left "left")
                                                   (and right "right")))))
    (run-program-to-string "toilet" (nconc
                                     (list "-f" *font* "-w" width)
                                     (when (plusp (length filter)) (list "-F" filter))
                                     (list (prin1-to-string text))))))

(defun demo-fonts ()
  (iter (for font in *fonts* )
    (format t "~A~%~%~A~%~%" (white font) (ascii-text "Hello!" :font font))))

(defun indent-paragraph (text spaces &optional (char #\space))
  (let ((lines (split-sequence #\newline text))
        (indent (make-string spaces :initial-element char)))
    (with-output-to-string (stream)
      (iter (for els on lines)
        (princ indent stream)
        (princ (car els) stream)
        (when (cdr els) (terpri stream))))))

;;; from https://www.rosettacode.org/wiki/Word_wrap#Common_Lisp

(defun word-wrap (str width)
  (setq str (concatenate 'string str " ")) ; add sentinel
  (do* ((len (length str))
        (lines nil)
        (begin-curr-line 0)
        (prev-space 0 pos-space)
        (pos-space (position #\Space str) (when (< (1+ prev-space) len) (position #\Space str :start (1+ prev-space)))))
       ((null pos-space) (progn (push (subseq str begin-curr-line (1- len)) lines) (nreverse lines)))
    (when (> (- pos-space begin-curr-line) width)
      (push (subseq str begin-curr-line prev-space) lines)
            (setq begin-curr-line (1+ prev-space)))))
