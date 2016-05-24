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

(defun print-table (rows &key (stream *standard-output*) (gap "  ") (align :left))
  (let* ((rows (iter (for row in rows) (collect (ensure-list row))))
         (max-row-length (apply #'max (mapcar #'length rows)))
         (base-widths (mapcar (lambda (row) (maximize-length row :key 'princ-to-string :length 'length-mono))
                              (rotate-rows-to-columns rows)))
         (control-string (concatenate 'string "~~~D" (ecase align (:right "@") (:left "")) "A")))
    (iter
      (for row in (mapcar (lambda (row) (pad-list row max-row-length "")) rows))
      (iter (for els on row)
        (for width in base-widths)
        (let ((column (car els)))
          (format stream (format nil (concatenate 'string control-string (when (cdr els) gap))
                                 (+ width (control-length column))) column)))
      (terpri stream))))

(defun print-selection-table (listvar selectvar &key (columns 4) (selection-color :cyan))
  (let* ((list (symbol-value listvar))
         (numspace (1+ (floor (log (length list) 10))))
         (numprint (format nil "~~~AD. ~~A" numspace))
         (current (symbol-value selectvar)))
    (print-table
     (group
      (iter (for el in (symbol-value listvar))
        (for count from 1)
        (collect (format nil numprint count
                         (if (equal current el)
                             (with-output-to-string (stream)
                               (with-color (selection-color :stream stream :effect :bright)
                                 (princ el stream)))
                             el))))
      columns))))

(defmacro define-selection-menu (name type list selection)
  `(defun ,name (&optional select)
     (cond
       (select
        (unless (and (> select 0) (< select (length ,list)))
          (error ,(format nil "Invalid ~(~A~) index ~~A." type) select))
        (setf ,selection (nth (1- select) ,list))
        (format t ,(format nil "Using ~(~A~) ~~S." type) ,selection))
       (t (print-selection-table ',list ',selection)))))

