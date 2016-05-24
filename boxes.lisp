(in-package :cl-ascii-art)

(defun boxed (text &key (design "c") (stream *standard-output*))
  (run-program-on-data "boxes" (list "-d" design) text stream)
  (values))

(defparameter *cow-directory* "/usr/share/cowsay/cows/")

(defvar *cows* nil)
(defvar *current-cow* "default")

(define-selection-menu cows cow *cows* *current-cow*)

(defun load-cows ()
  (setf *cows* (directory-filenames *cow-directory* '("cow"))))

(defun cowsay (text &key (design *current-cow*) (stream *standard-output*))
  (unless (member design *cows* :test 'equalp)
    (error "Unknow cow design ~S." design))
  (run-program-on-data "cowsay" (list "-f" design) text stream)
  (values))
