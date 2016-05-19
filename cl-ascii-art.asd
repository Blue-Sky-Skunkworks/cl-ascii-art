(defsystem "cl-ascii-art"
  :description "Ascii Art generating routines."
  :version "0.1"
  :author "William Halliburton"
  :license "GPLv3"
  :serial t
  :depends-on ("cl-ansi-text")
  :components ((:static-file "cl-ascii-art.asd")
               (:file "package")
               (:file "utility")
               (:file "fractals")
               (:file "hilbert-space-filling-curve")
               (:file "text")))
