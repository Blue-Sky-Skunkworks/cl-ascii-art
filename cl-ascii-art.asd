(defsystem "cl-ascii-art"
  :description "Ascii Art generating routines."
  :version "0.1"
  :author "William Halliburton"
  :license "GPLv3"
  :serial t
  :depends-on ("cl-ansi-text" "alexandria" "iterate" "inferior-shell"
                              "split-sequence" "cl-ppcre")
  :components ((:static-file "cl-ascii-art.asd")
               (:file "package")
               (:file "utility")
               (:file "note")
               (:file "color")
               (:file "tables")
               (:file "demos")
               (:file "fractals")
               (:file "hilbert-space-filling-curve")
               (:file "bitmaps")
               (:file "fonts")
               (:file "text")
               (:file "boxes")
               (:file "heart")
               (:file "unicode")
               (:file "markdown")
               (:file "border")
               (:file "readme")))
