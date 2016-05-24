(in-package :cl-ascii-art)

(defun print-demo (&key (stream *standard-output*)
                     listvar render-fn
                     count max-height min-height max-width min-width)
  (iter (for el in (symbol-value listvar))
    (for index from 1 to (or count most-positive-fixnum))
    (let* ((demo (with-output-to-string (*standard-output*)
                   (funcall render-fn el)))
           (count (line-count demo))
           (line-width (line-width demo)))
      (when (and (or (null max-height) (<= count max-height))
                 (or (null min-height) (>= count min-height))
                 (or (null max-width) (<= line-width max-width))
                 (or (null min-width) (>= line-width min-width)))
        (format stream "~%~A~%~%~A" (white el) demo)))))

(defmacro define-demo (name args listvar &body body)
  `(defun ,(symb 'demo- name) (&key ,@args (stream *standard-output*)
                                 count max-height min-height max-width min-width)
     (print-demo :stream stream
                 :listvar ',listvar
                 :render-fn (lambda (el) ,@body)
                 :count count
                 :max-height max-height
                 :min-height min-height
                 :max-width max-width
                 :min-width min-width)))

(define-demo fonts ((text "AbBbCc123!@#") (width 120) full-width) *fonts*
  (text text :font el :width width :full-width full-width))

