
(defpackage cl-ascii-art
  (:nicknames art)
  (:use common-lisp iterate split-sequence cl-ppcre inferior-shell)
  (:import-from cl-ansi-text with-color black red green yellow blue magenta cyan white)
  (:import-from uiop directory-files subdirectories)
  (:import-from split-sequence split-sequence)
  (:import-from alexandria if-let when-let ensure-list with-input-from-file shuffle
                with-output-to-file assoc-value random-elt)
  (:export with-macrolets with-flets as-string
           text indent-text word-wrap fractal hilbert-space-filling-curve unfill-paragraph text-3d
           heading print-with-ellipses remove-trailing-newline
           black red green yellow blue magenta cyan white with-color if-color
           fonts select-font boxed cowsay
           print-table table-element-string *table-float-precision*
           *table-element-max-length*
           clock-face show-unicode-characters unicode-apropos print-in-braille
           text-with-unicode-boxes
           demo-fonts demo-boxes demo-cows
           *inhibit-note* *show-note-clock* note bugout)
  (:documentation "
    _    ____   ____ ___ ___      _         _      __               ____ _
   / \  / ___| / ___|_ _|_ _|    / \   _ __| |_   / _| ___  _ __   / ___| |
  / _ \ \___ \| |    | | | |    / _ \ | '__| __| | |_ / _ \| '__| | |   | |
 / ___ \ ___) | |___ | | | |   / ___ \| |  | |_  |  _| (_) | |    | |___| |___
/_/   \_\____/ \____|___|___| /_/   \_\_|   \__| |_|  \___/|_|     \____|_____|
"))

(defpackage cl-ascii-art-markdown
  (:nicknames artmd)
  (:use common-lisp cl-ascii-art iterate split-sequence)
  (:export markdown))

(defpackage cl-ascii-art-readme
  (:use common-lisp iterate cl-ascii-art-markdown)
  (:import-from alexandria with-output-to-file)
  (:export render-readme))


