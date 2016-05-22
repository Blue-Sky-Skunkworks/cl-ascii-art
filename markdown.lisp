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
  `(format t "~A~%~A~%~%" ,string ,(make-string (length string) :initial-element #\=)))

(defmacro h2 (string)
  `(format t "~A~%~A~%~%" ,string ,(make-string (length string) :initial-element #\-)))

(defmacro define-headers ()
  `(progn
     ,@(iter (for index from 3 to 6)
         (collect
             `(defmacro ,(symb 'h index) (string)
                `(format t ,,(format nil "~A ~~A~~%~~%" (make-string index :initial-element #\#)) ,string))))))

(define-headers)

(defmacro br (&optional (count 1))
  `(dotimes (x ,count) (write-char #\newline)))

(defmacro fractal (&rest args)
  `(art:fractal ,@args))

(defmacro hilbert-space-filling-curve  (&optional (order 6))
  `(art:hilbert-space-filling-curve :order ,order))

(defmacro text (&rest args)
  `(art:text ,@args))

(defmacro pre (&rest args)
  `(art:indent-text (test-markdown ,@args) 4))

(defmacro markdown (&body body)
  (with-macrolets '(h1 h2 h3 h4 h5 h6 br text pre fractal hilbert-space-filling-curve)
    `(with-output-to-string (*standard-output*)
       ,@(iter (for el in body)
           (collect
               (typecase el
                 (string `(princ ,el))
                 (t el)))))))

(defmacro test-markdown (&body body)
  `(with-output-to-string (*standard-output*)
     ,@(iter (for el in body)
         (collect
             (typecase el
               (string `(princ ,el))
               (t el))))))

(test-markdown (pre (text "Hello")))

