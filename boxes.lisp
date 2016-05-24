(in-package :cl-ascii-art)

(defun boxed (text &key (design "c") (stream *standard-output*))
  (run-program-on-data "boxes" (list "-d" design) text stream)
  (values))

(defparameter *cow-directory* "/usr/share/cowsay/cows/")
(defvar *cows* nil)
(defvar *current-cow* "default")

(defun load-cows ()
  (setf *cows* (directory-filenames *cow-directory* '("cow"))))

(defun cows (&optional select)
  (cond
    (select
     (unless (and (> select 0) (< select (length *cows*)))
       (error "Invalid cow index ~A." select))
     (setf *current-cow* (nth (1- select) *cows*))
     (format t "Using cow ~S." *current-cow*))
    (t (let ((index 0))
         (iter (for row in (group *cows* 4))
           (iter (for name in row)
             (if (string= name *current-cow*)
                 (format t "~2D  ~26A  " (incf index) (cyan name :effect :bright))
                 (format t "~2D  ~15A  " (incf index) name)))
           (terpri)))))))

(defun cowsay (text &key (design *current-cow*) (stream *standard-output*))
  (unless (member design *cows* :test 'equalp)
    (error "Unknow cow design ~S." design))
  (run-program-on-data "cowsay" (list "-f" design) text stream)
  (values))
