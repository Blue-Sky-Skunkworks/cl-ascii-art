(in-package :cl-ascii-art-markdown)

(defmacro h1 (string)
  `(write-string ,(format nil "~A~%~A~%~%" string (make-string (length string) :initial-element #\=))))

(defmacro h2 (string)
  `(write-string ,(format nil "~A~%~A~%~%" string (make-string (length string) :initial-element #\-))))

(defmacro define-headers ()
  `(progn
     ,@(iter (for index from 3 to 6)
         (collect
             `(defmacro ,(symb 'h index) (string)
                `(write-string ,(format nil ,(format nil "~A ~~A~~%~~%" (make-string index :initial-element #\#)) string)))))))

(define-headers)

(defmacro br (&optional (count 1))
  `(dotimes (x ,count) (write-char #\newline)))

(defmacro pre (&rest args)
  `(indent-text (%markdown ,@args) 4))

(defmacro %markdown (&body body)
  `(with-output-to-string (*standard-output*)
     ,@(iter (for el in body)
         (collect
             (typecase el
               (string `(princ ,el))
               (t el))))))

(defmacro markdown (&body body)
  (with-macrolets '(h1 h2 h3 h4 h5 h6 br pre)
    (with-flets '(text fractal hilbert-space-filling-curve)
      `(with-output-to-string (*standard-output*)
         ,@(iter (for el in body)
             (collect
                 (typecase el
                   (string `(princ ,el))
                   (t el))))))))



(%markdown (pre (text "Hello") (br) (text "yo")))

