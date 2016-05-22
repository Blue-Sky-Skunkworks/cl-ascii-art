
(defpackage cl-ascii-art
  (:nicknames art)
  (:use common-lisp iterate split-sequence)
  (:import-from cl-ansi-text with-color black red green yellow blue magenta cyan white)
  (:import-from uiop directory-files subdirectories)
  (:import-from split-sequence split-sequence)
  (:import-from alexandria ensure-list)
  (:export text indent-text word-wrap fractal hilbert-space-filling-curve unfill-paragraph
           heading print-with-ellipses
           black red green yellow blue magenta cyan white with-color if-color
           fonts select-font demo-fonts)
  (:documentation "
    _    ____   ____ ___ ___      _         _      __               ____ _
   / \  / ___| / ___|_ _|_ _|    / \   _ __| |_   / _| ___  _ __   / ___| |
  / _ \ \___ \| |    | | | |    / _ \ | '__| __| | |_ / _ \| '__| | |   | |
 / ___ \ ___) | |___ | | | |   / ___ \| |  | |_  |  _| (_) | |    | |___| |___
/_/   \_\____/ \____|___|___| /_/   \_\_|   \__| |_|  \___/|_|     \____|_____|
"))

(defpackage cl-ascii-art-markdown
  (:nicknames artmd)
  (:use common-lisp iterate split-sequence cl-interpol)
  (:export markdown))

(defpackage cl-ascii-art-markdown.testing
  (:use common-lisp iterate cl-ascii-art-markdown))

(defpackage cl-ascii-art-readme
  (:use common-lisp iterate cl-ascii-art-markdown)
  (:import-from alexandria with-output-to-file)
  (:export render-readme))


