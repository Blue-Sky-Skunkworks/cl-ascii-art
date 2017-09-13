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

(defparameter *table-float-precision* 4)

(defparameter *table-element-max-length* nil)

(defmethod table-element-string (obj)
  (princ-to-string obj))

(defmethod table-element-string ((obj string))
  (white
   (if *table-element-max-length*
       (as-string (print-with-ellipses obj :max *table-element-max-length*))
       obj)))

(defmethod table-element-string ((obj symbol))
  (cyan (princ-to-string obj)))

(defmethod table-element-string ((obj pathname))
  (red (princ-to-string obj)))

(defmethod table-element-string ((obj number))
  (blue (format nil "~:D" obj) :effect :bright))

(defmethod table-element-string ((obj float))
  (blue (format nil (format nil "~~,~AF" *table-float-precision*) obj) :effect :bright))

(defun print-table (rows &key (stream *standard-output*) (gap "  ") (align :left) hilight
                           headings total page-size)
  (when rows
    (let* ((total (and total
                       (let ((sum (make-list (length (first rows)) :initial-element 0)))
                         (iter (for row in rows)
                           (iter (for col in row)
                             (for i from 0)
                             (when (member i total :test 'eql)
                               (incf (nth i sum) col))))
                         (substitute "" 0 sum))))
           (rows (append (and headings (list headings))
                         (iter (for row in rows) (collect (ensure-list row)))
                         (and total (list total))))
           (num-rows (length rows))
           (max-row-length (apply #'max (mapcar #'length rows)))
           (base-widths (mapcar (lambda (row) (maximize-length row :key 'table-element-string :length 'length-mono))
                                (rotate-rows-to-columns rows)))
           (control-string (concatenate 'string "~~~D" (ecase align (:right "@") (:left "")) "A"))
           (control-string-last (concatenate 'string "~" (ecase align (:right "@") (:left "")) "A")))
      (iter
        (for row in (mapcar (lambda (row) (pad-list row max-row-length "")) rows))
        (for rowi from 1)
        (iter (for els on row)
          (for width in base-widths)
          (let* ((column (car els))
                 (column-string (table-element-string column))
                 (row
                   (format nil
                           (if (cdr els)
                               (format nil (concatenate 'string control-string gap)
                                       (+ width (control-length column-string)))
                               control-string-last)
                           column-string)))
            (cond
              ((and headings (= rowi 1))
               (write-string (white (white row :effect :underline) :effect :bright) stream))
              ((and hilight (funcall hilight rowi))
               (write-string (white row :effect :bright) stream))
              (t (write-string row stream)))))
        (when (and page-size (zerop (mod rowi page-size)))
          (format t "~&<~A more>" (- num-rows rowi))
          (force-output)
          (when (char= (read-char) #\q) (return-from print-table)))
        (terpri stream)))))

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

