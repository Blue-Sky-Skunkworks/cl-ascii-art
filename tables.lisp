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
         (control-string (concatenate 'string "~~~D" (ecase align (:right "@") (:left "")) "A"))
         (control-string-last (concatenate 'string "~" (ecase align (:right "@") (:left "")) "A")))
    (iter
      (for row in (mapcar (lambda (row) (pad-list row max-row-length "")) rows))
      (iter (for els on row)
        (for width in base-widths)
        (let ((column (car els)))
          (format stream
                  (if (cdr els)
                      (format nil (concatenate 'string control-string gap)
                              (+ width (control-length (etypecase column
                                                         (string column)
                                                         (t (princ-to-string column))))))
                      control-string-last)
                  column)))
      (terpri stream))))

(defun print-selection-table (listvar selectvar &key (reader 'identity) (columns 5) (selection-color :cyan))
  (let ((current (symbol-value selectvar)) ncols)
    (print-table
     (group
      (iter outer (for el in (symbol-value listvar))
        (for count from 1)
        (collect count)
        (let ((els (ensure-list (funcall reader el))))
          (when (first-iteration-p) (setf ncols (length els)))
          (iter (with selected = (equal current el))
            (for col in els)
            (in outer (collect
                          (if selected
                              (with-output-to-string (stream)
                                (with-color (selection-color :stream stream :effect :bright)
                                  (princ col stream)))
                              (princ-to-string col)))))))
      (* columns (1+ ncols))))))

(defmacro define-selection-menu (name (type list selection &key default reader args) &body init)
  `(progn
     (defvar ,list nil)
     (defvar ,selection ,default)
     (defun ,(symb 'load- name) () (setf ,list ,@init))
     (defun ,(symb 'ensure- name '-loaded) () (unless ,list (,(symb 'load- name))))
     (defun ,(symb 'select- type) (index)
       (,(symb 'ensure- name '-loaded))
       (unless (and (> index 0) (<= index (length ,list)))
         (error ,(format nil "Invalid ~(~A~) index ~~A." type) index))
       (setf ,selection (nth (1- index) ,list))
       (format t ,(format nil "Using ~(~A~) ~~S." type) ,selection))
     (defun ,name (,@args)
       (,(symb 'ensure- name '-loaded))
       (print-selection-table ',list ',selection ,@(when reader `(:reader ,reader))))))

