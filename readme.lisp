(in-package :cl-ascii-art-readme)

(defun render-readme ()
  (markdown
    (h1 "Once apon a time...")
    (h2 "there was a boy.")
    (pre (text "He set out alone," :font "mini")
         (br)
         (text "and would not be boxed in." :font "wideterm" :border t))
    (br 3)
    (h3 "But he had *many* friends...")
    (h4 "He liked fractals!")
    (pre (fractal 186 3 3 :char #\black_diamond))
    (br 2)
    (h4 "and ∞ ∪ ∞")
    (pre (hilbert-space-filling-curve))))

(defun save-readme ()
  (with-output-to-file (stream (art::art-file "README.md") :if-exists :supersede :if-does-not-exist :create)
    (princ (render-readme) stream))
  (values))

