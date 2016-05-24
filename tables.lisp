(in-package :cl-ascii-art)

(defun rotate-rows-to-columns (rows)
  (loop for remaining = rows then (mapcar #'cdr remaining)
        while (not (every #'null remaining))
        collect (mapcar #'car remaining)))

(defun maximize-length (list &key (key #'identity) (length #'length))
  (iter (for element in list) (maximizing (funcall length (funcall key element)))))

(defun pad-list (list length &optional (pad-element nil))
  (iter
    (for el on list)
    (for x from 1)
    (when (null (cdr el))
      (setf (cdr el) (make-list (- length x) :initial-element pad-element))
      (return list))))

(defun print-table (rows &key (gap "  ") (align :left))
  (let* ((rows (iter (for row in rows) (collect (ensure-list row))))
         (max-row-length (apply #'max (mapcar #'length rows)))
         (control-string (format nil
                                 (concatenate
                                  'string "~{~~~D" (ecase align (:right "@") (:left "")) "A~^" gap "~}~%")
                                 (mapcar (lambda (row) (maximize-length row :key 'princ-to-string :length 'length-mono))
                                         (rotate-rows-to-columns rows)))))
    (iter
      (for row in (mapcar (lambda (row) (pad-list row max-row-length "")) rows))
      (apply #'format t control-string row))))

