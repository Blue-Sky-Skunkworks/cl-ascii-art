(in-package :cl-ascii-art)

(defun size-filter (max-height min-height max-width min-width)
  (lambda (name el)
    (declare (ignore name))
    (and el
         (if (or max-height min-height max-width min-width)
             (let ((count (line-count el))
                   (line-width (line-width el)))
               (when (and (or (null max-height) (<= count max-height))
                          (or (null min-height) (>= count min-height))
                          (or (null max-width) (<= line-width max-width))
                          (or (null min-width) (>= line-width min-width)))
                 el))
             el))))

(defun case-filter (lower-case)
  (lambda (name el)
    (and el
         (if lower-case
             (when (font-case name) el)
             el))))

(defun print-demo (&key (stream *standard-output*) generator filter count)
  (iter
    (for index from 1 to (or count most-positive-fixnum))
    (multiple-value-bind (value remaining) (funcall generator)
      (destructuring-bind (name . demo) value
        (when (or (null filter) (funcall filter name demo))
          (format stream "~%~A~%~%~A" (white name) demo)))
      (while remaining))))

(defmacro define-demo (name args (&key generator listvar filter) &body body)
  `(defun ,(symb 'demo- name) (&key ,@args (stream *standard-output*)
                                 count max-height min-height max-width min-width)
     (let ((size-filter (size-filter max-height min-height max-width min-width))
           ,@(when filter `((filter ,filter))))
      (print-demo :stream stream
                  :generator, (if generator generator
                                  `(make-list-fn-generator
                                    ,listvar
                                    (lambda (el)
                                      (cons
                                       el
                                       (handler-case
                                           (with-output-to-string (*standard-output*)
                                             ,@body)
                                         (error (c) (warn "Demo errored ~S ~S ~A." ',name el c)))))))
                  :count count
                  :filter ,(if filter
                               `(lambda (name el) (funcall size-filter name (funcall filter name el)))
                               'size-filter)))))

(define-demo fonts ((text "AaBbCc123!@#") (width 120) full-width lower-case)
  (:listvar *fonts*
   :filter (case-filter lower-case))
  (text text :font el :width width :full-width full-width))

(define-demo cows ((text "AaBbCc123!@#")) (:listvar *cows*) (cowsay text :design el))
(define-demo boxes ((text "AaBbCc123!@#")) (:listvar *boxes*) (boxed text :design el))


