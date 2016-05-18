(defpackage cl-ascii-art
  (:nicknames aa)
  (:use common-lisp iterate)
  (:import-from cl-ansi-text with-color black red green yellow blue magenta cyan white)
  (:import-from uiop directory-files subdirectories)
  (:export text)
  (:documentation ""))
