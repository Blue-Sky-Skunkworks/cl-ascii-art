(in-package :cl-ascii-art)

(defun boxed (text &key (design "c") (stream *standard-output*))
  (run-program-on-data "boxes" (list "-d" design) text stream)
  (values))
