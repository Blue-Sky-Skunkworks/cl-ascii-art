(in-package :cl-ascii-art)

(defparameter *boxes-config* "/etc/boxes/boxes-config")
(define-selection-menu boxes box *boxes* *current-box* "c"
  (sort (collect-scans "BOX (.+)" (slurp-file *boxes-config*)) #'string<))

(defun boxed (text &key (design *current-box*) (stream *standard-output*))
  (run `(pipe (echo ,text) (boxes -d ,design)) :output stream))

(defparameter *cow-directory* "/usr/share/cowsay/cows/")
(define-selection-menu cows cow *cows* *current-cow* "default"
  (directory-filenames *cow-directory* '("cow")))

(defun cowsay (text &key (design *current-cow*) (stream *standard-output*))
  (unless (member design *cows* :test 'equalp)
    (error "Unknow cow design ~S." design))
  (run `(pipe (echo ,text) (cowsay -f ,design)) :output stream))
