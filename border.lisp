(in-package :cl-ascii-art)

(defun border (&key (stream *standard-output*) (width 60) (height 52) (hborder 4) (vborder 2) (chars "."))
  (let ((list (coerce chars 'list)))
    (iter (for row from 0 to height)
      (iter (for col from 0 to width)
        (princ
         (if (or (< col hborder) (< row vborder)
                 (> col (- width hborder)) (> row (- height vborder)))
             (random-elt chars)
             #\space)
         stream))
      (terpri stream))))
