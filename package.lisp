(defpackage cl-ascii-art
  (:nicknames aa)
  (:use common-lisp iterate split-sequence)
  (:import-from cl-ansi-text with-color black red green yellow blue magenta cyan white)
  (:import-from uiop directory-files subdirectories)
  (:import-from split-sequence split-sequence)
  (:export text indent-text word-wrap)
  (:documentation "
    _    ____   ____ ___ ___      _         _      __               ____ _
   / \  / ___| / ___|_ _|_ _|    / \   _ __| |_   / _| ___  _ __   / ___| |
  / _ \ \___ \| |    | | | |    / _ \ | '__| __| | |_ / _ \| '__| | |   | |
 / ___ \ ___) | |___ | | | |   / ___ \| |  | |_  |  _| (_) | |    | |___| |___
/_/   \_\____/ \____|___|___| /_/   \_\_|   \__| |_|  \___/|_|     \____|_____|
"))
