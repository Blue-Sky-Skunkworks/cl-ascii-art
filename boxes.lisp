(in-package :cl-ascii-art)

(defparameter *boxes-config* "/etc/boxes/boxes-config")
(define-selection-menu boxes (box *boxes* *current-box* :default "c")
  (sort (collect-scans "BOX (.+)" (slurp-file *boxes-config*)) #'string<))

(defun boxed (text &key (design *current-box*) (stream *standard-output*))
  (ensure-boxes-loaded)
  (run `(pipe (echo ,text) (boxes -d ,design)) :output stream))

(defparameter *cow-directory* "/usr/share/cowsay/cows/")
(define-selection-menu cows (cow *cows* *current-cow* :default "default")
  (directory-filenames *cow-directory* '("cow")))

(defun cowsay (text &key (word-wrap t) (design *current-cow*) (stream *standard-output*))
  (ensure-cows-loaded)
  (unless (member design *cows* :test 'equalp)
    (error "Unknow cow design ~S." design))
  (run `(pipe (echo ,text) (cowsay ,@(unless word-wrap '(-n)) -f ,design)) :output stream))
