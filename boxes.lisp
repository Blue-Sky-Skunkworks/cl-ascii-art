(in-package :cl-ascii-art)

(defparameter *boxes-config* "/etc/boxes/boxes-config")

(defvar *boxes* nil)
(defvar *current-box* "c")

(define-selection-menu boxes box *boxes* *current-box*)

(defun load-boxes ()
  (let ((text (slurp-file *boxes-config*)) acc)
    (do-scans (ms me rs re (create-scanner "BOX .+") text)
      (push (string-trim '(#\space) (subseq text (+ ms 4) me)) acc))
    (setf *boxes* (sort acc #'string<))))

(defun boxed (text &key (design *current-box*) (stream *standard-output*))
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
