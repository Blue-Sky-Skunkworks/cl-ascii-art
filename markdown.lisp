(in-package :cl-ascii-art-markdown)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (when a (princ a s)))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

;;; Adapted from cl-dsl. Props out to Alexander Popolitov
;;; <popolit@gmail.com> for unleasing a wave of easily written DLS's
;;; onto my world with such a simple set of macros.

  (defun with-macrolets (would-be-macrolets body)
    `(macrolet ,(mapcar (lambda (x)
                          `(,(intern (string x)) (&rest args) `(,',x ,@args)))
                 would-be-macrolets)
       ,body))

  (defun with-flets (would-be-flets body)
    `(flet ,(mapcar (lambda (x)
                      `(,(intern (string x)) (&rest args) `(,',x ,@args)))
             would-be-flets)
       ,body)))

(defmacro h1 (string)
  `(format stream "~A~%~A~%~%" ,string ,(make-string (length string) :initial-element #\=)))

(defmacro h2 (string)
  `(format stream "~A~%~A~%~%" ,string ,(make-string (length string) :initial-element #\-)))

(defmacro define-headers ()
  `(progn
     ,@(iter (for index from 3 to 6)
         (collect
             `(defmacro ,(symb 'h index) (string)
                `(format stream ,,(format nil "~A ~~A~~%~~%" (make-string index :initial-element #\#)) ,string))))))

(define-headers)

(defmacro br (&optional (count 1))
  `(dotimes (x ,count) (write-char #\newline stream)))

(defmacro fractal (&rest args)
  (destructuring-bind (n r g &rest rest) args
    `(princ (with-output-to-string (str)
              (art:fractal ,n ,r ,g :stream str ,@rest)) stream)))

(defmacro hilbert-space-filling-curve  (&optional (order 6))
  `(princ (with-output-to-string (str)
            (art:hilbert-space-filling-curve :stream str :order ,order)) stream))

(defmacro text (&rest args)
  `(let ((*standard-output* stream)) (art:text ,@args))))

(defmacro markdown (&body body)
  (with-macrolets '(h1 h2 h3 h4 h5 h6 br text fractal hilbert-space-filling-curve)
    `(with-output-to-string (stream)
       ,@body)))

(defmacro tmarkdown (&body body)
  `(with-output-to-string (stream)
     ,@(iter (for el in body)
         (collect
             (typecase el
               (string `(princ ,el stream))
               (t el))))))

;; (princ
;;  (tmarkdown
;;    (h1 "CL-ASCII-ART")
;;    (br 3)
;;    (h1 "Once apon a time...")
;;    (h2 "there was a boy.")
;;    (text "He set out alone," :font "mini")
;;    (br)
;;    (text "and would not be boxed in." :font "wideterm" :border t)
;;    (br 3)
;;    (h3 "But he had *many* friends...")
;;    (h4 "He liked fractals!")
;;    (fractal 186 3 3 :char #\black_diamond)
;;    (br 2)
;;    (h4 "and ∞ ∪ ∞")
;;    (hilbert-space-filling-curve)



;;    ))


(in-package :cl-ascii-art-markdown.testing)


;; (princ
;;  (markdown
;;    (h1 "CL-ASCII-ART")
;;    (br 3)
;;    (h1 "Once apon a time...")
;;    (h2 "there was a boy.")
;;    (text "He set out alone," :font "mini")
;;    (br)
;;    (text "and would not be boxed in." :font "wideterm" :border t)
;;    (br 3)
;;    (h3 "But he had *many* friends...")
;;    (h4 "He liked fractals!")
;;    (fractal 186 3 3 :char #\black_diamond)
;;    (br 2)
;;    (h4 "and ∞ ∪ ∞")
;;    (hilbert-space-filling-curve)



;;    ))

;; (princ
;;  (markdown
;;    (h1 "asd")
;;    ))

