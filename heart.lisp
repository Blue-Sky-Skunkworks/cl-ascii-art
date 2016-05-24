(in-package :cl-ascii-art)

(defun heart (&optional (num 120))
  "Draw a heart."
  (labels ((r (v) (+ 17 (round v)))
           (y (p) (r (* 16 (expt (sin p) 3))))
           (x (p) (round (* 0.75 (r (- (* 13 (cos p)) (*  5 (cos (* 2 p))) (* 2 (cos (* 3 p))) (cos (* 4 p))))))))
    (let ((arr (make-array '(34 34)))
          (dp (/ (* 2 PI) num)))
      (loop
         for index from 0 to num
         do (let ((p (* dp index))) (setf (aref arr (x p) (y p)) t)))
      (loop for row from 22 downto 0
           do (loop
                 for col from 0 to 33
                 do (format t "~A" (if (eq t (aref arr row col)) #\white_heart_suit #\space)))
           do (terpri)))))
