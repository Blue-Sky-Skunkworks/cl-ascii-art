(in-package :cl-ascii-art)

(defun print-demo (&key (stream *standard-output*)
                     generator
                     count max-height min-height max-width min-width)
  (iter
    (for index from 1 to (or count most-positive-fixnum))
    (multiple-value-bind (value remaining) (funcall generator)
      (destructuring-bind (name . demo) value
        (let ((count (line-count demo))
              (line-width (line-width demo)))
          (when (and (or (null max-height) (<= count max-height))
                     (or (null min-height) (>= count min-height))
                     (or (null max-width) (<= line-width max-width))
                     (or (null min-width) (>= line-width min-width)))
            (format stream "~%~A~%~%~A" (white name) demo))))
      (while remaining))))

(defmacro define-demo (name args listvar &body body)
  `(defun ,(symb 'demo- name) (&key ,@args (stream *standard-output*)
                                 count max-height min-height max-width min-width)
     (print-demo :stream stream
                 :generator
                 (make-list-fn-generator
                  ,listvar
                  (lambda (el)
                    (cons
                     el
                     (handler-case
                         (with-output-to-string (*standard-output*)
                           ,@body)
                       (error (c) (warn "Demo errored ~S ~S ~A." ',name el c))))))
                 :count count
                 :max-height max-height
                 :min-height min-height
                 :max-width max-width
                 :min-width min-width)))

(define-demo fonts ((text "AbBbCc123!@#") (width 120) full-width) *fonts*
  (text text :font el :width width :full-width full-width))

(define-demo cows ((text "AbBbCc123!@#")) *cows* (cowsay text :design el))
(define-demo boxes ((text "AbBbCc123!@#")) *boxes* (boxed text :design el))

