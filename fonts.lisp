(in-package :cl-ascii-art)

(defclass font ()
  ((name :initarg :name :reader name)
   (height :initarg :height :reader height)
   (baseline :initarg :baseline :reader baseline)
   (max-length :initarg :max-length :reader max-length)
   (old-layout :initarg :old-layout :reader old-layout)
   (comment-lines :initarg :comment-lines :reader comment-lines)
   (print-direction :initarg :print-direction :reader print-direction)
   (full-layout :initarg :full-layout :reader full-layout)
   (code-count :initarg :code-count :reader code-count)
   (comments :initarg :comments :reader comments)
   lower-case))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t)
    (format stream "~A" (name font))))

(defmethod lower-case ((font font))
  (if (slot-boundp font 'lower-case)
      (slot-value font 'lower-case)
      (setf (slot-value font 'lower-case)
            (let* ((upper (as-string (text "A" :font font)))
                   (lower (as-string (text "a" :font font))))
              (not (string= upper lower))))))

(defun parse-font-file (path)
  (with-input-from-file (stream path)
    (when-let (line (handler-case
                        (string-trim '(#\return) (read-line stream))
                      (error (c) (warn "Error reading font file ~S ~A." path c)) ))
      (unless (or (string-starts-with line "flf2a")
                  (string-starts-with line "tlf2a"))
        (error "Invalid font file ~S." path))
      (let ((hardblank (char line 5)))
        (destructuring-bind (height baseline max-length old-layout comment-lines &optional print-direction full-layout code-count)
            (iter (for el in (split-sequence #\space line :start 6 :remove-empty-subseqs t))
              (for x from 1 to 8)
              (collect (parse-integer el)))
          (make-instance 'font :name (pathname-name path)
                               :height height :baseline baseline :max-length max-length
                               :old-layout old-layout :comment-lines comment-lines
                               :print-direction print-direction :full-layout full-layout
                               :code-count code-count
                               :comments (ignore-errors
                                          (iter (for x from 1 to comment-lines)
                                            (collect (string-trim '(#\return) (read-line stream)))))))))))

(defun load-font-directory (&optional (path *font-directory*))
  (iter (for file in (directory-files path))
    (when (member (pathname-type file) '("flf" "tlf") :test 'equal)
      (when-let ((font (parse-font-file file)))
        (collect font)))))

(defparameter *font-directory* (art-file "fonts/"))

(define-selection-menu fonts (font *fonts* *font* :args (&key detail)
                                                  :reader (lambda (font)
                                                            (nconc
                                                             (when detail (list (height font) (max-length font)))
                                                             (list (name font)))))
  (load-font-directory))

(defun sort-fonts (&optional (key 'height))
  (setf *fonts* (sort *fonts* (lambda (a b) (< (or a 0) (or b 0))) :key key)))

(defun find-font-from-name (name)
  (or (car (member name *fonts* :test 'equal :key 'name))
      (error "Unknown font ~S." name)))

(defun find-font (name-or-index)
  (typecase name-or-index
    (font name-or-index)
    (string (find-font-from-name name-or-index))
    (integer
     (if (or (< name-or-index 1) (> name-or-index (length *fonts*)))
         (error "Ascii font index out of range.")
         (nth (1- name-or-index) *fonts*)))))

(defun strings-to-bitmap (strings height sx sy)
  (let ((width (* (length (first strings)) 4))
        (ch (length strings)))
    (let ((bitmap (make-bitmap width height)))
      (iter (for row in strings)
        (for y from (- height sy ch))
        (let ((val (parse-integer row :radix 16)))
          (iter
            (for x from sx to (1- width))
            (setf (aref bitmap y x) (logbitp (- (1- width) (- x sx)) val)))))
      (values bitmap width))))

(defun load-bitmap-font (&key (filename (art-file "data/UnifontMedium-16.bdf"))
                           (range '(32 126)))
  (with-input-from-file (stream filename)
    (let (in-bitmap bitmap (font (make-hash-table)) name bbx height)
      (iter
        (let* ((line (read-line stream nil))
               (*read-eval* nil)
               (cmd (unless in-bitmap (and line (read-from-string line)))))
          (cond
            ((null line) (return))
            ((eq cmd 'cap_height) (setf height (parse-integer (subseq line 11))))
            ((eq cmd 'bbx) (setf bbx (with-input-from-string (str (subseq line 4))
                                       (iter (repeat 4) (collect (read str))))))
            ((eq cmd 'startchar)
             (setf name
                   (code-char (parse-integer (subseq line 12) :radix 16))
                   bitmap nil bbx nil))
            (in-bitmap
             (cond
               ((string= line "ENDCHAR")
                (when (and (>= (char-code name) (first range))
                           (<= (char-code name) (second range)))
                  (multiple-value-bind (bitmap width)
                      (strings-to-bitmap (nreverse bitmap) 16 (third bbx) (+ 2 (fourth bbx)))
                    (setf (gethash name font)
                          (list width bbx bitmap))))
                (setf in-bitmap nil))
               (t (push line bitmap))))
            ((eq cmd 'bitmap) (setf in-bitmap t)))))
      font)))

(defparameter *bitmap-font* (load-bitmap-font :range '(#x0000 #xFFFF)))

(defun print-font (&optional (font *bitmap-font*))
  (iter (for (k v) in-hashtable font)
    (format t "> ~A ~S~%" k k)
    (draw :bitmap (third v))
    (terpri)))

(defun print-font-grid (&key (width 16) (font *bitmap-font*))
  (let ((bitmap (make-bitmap (+ (* (1+ width) 16) 8) (+ (* (1+ width) 16) 2))))
    (iter (for x from 0 to (1- width))
          (set-pixel (+ 8 (* x 17)) 0 bitmap (format nil "0x~X" x)))
    (iter (for (k v) in-hashtable font)
          (for index from 0)
          (multiple-value-bind (y x) (floor index width)
            (cond
              ((>= y width)
               (draw :bitmap bitmap)
               (clear-bitmap bitmap)
               (setf index 0 x 0 y 0)))
            (when (= x 0)
              (set-pixel 0 (+ (* y 17) 2) bitmap (format nil "0x~X" (char-code k))))
            (copy-bitmap-onto-bitmap (third v) bitmap (+ (* x 17) 8) (+ (* y 17) 2))))
    (draw :bitmap bitmap)))

(defun save-font-grid ()
  (with-output-to-file (*standard-output* (art-file "generated/font.txt"))
    (print-font-grid)))

(defun bitmap-text (text &key (font *bitmap-font*))
  (multiple-value-bind (chars width)
      (iter (for char in-vector text)
        (let ((hit (or (gethash char font)
                       (warn "Missing font character ~S." char))))
          (summing (car hit) into width)
          (collect hit into bitmaps))
        (finally (return (values bitmaps width))))
    (let ((x 0))
      (with-bitmap (width 16)
        (iter (for (w bbx bitmap) in chars)
          (copy-bitmap-onto-bitmap bitmap *bitmap* x 0)
          (incf x w))
        *bitmap*))))
