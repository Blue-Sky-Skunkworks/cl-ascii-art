(in-package :cl-ascii-art)

(defun fractal-3-pattern (description)
  (apply 'logior
         (iter (for index from 0 to 8)
           (collect
               (if (not (char= #\space
                               (char description (+ index
                                                    (cond
                                                      ((> index 5) 2)
                                                      ((> index 2) 1)
                                                      (t 0))))))
                   (expt 2 index)
                   0)))))

;;; http://codegolf.stackexchange.com/questions/54453/generate-fractals-from-bit-patterns-in-ascii

(defun fractal (n-or-pattern r g &key (stream *standard-output*) (char #\#) &aux (s (expt r g)))
  (assert (or (not (stringp n-or-pattern)) (= r 3)))
  (let ((n (typecase n-or-pattern
             (string (fractal-3-pattern n-or-pattern))
             (t n-or-pattern))))
    (labels ((f (g x y s)
               (or (= g 0)
                   (multiple-value-bind (px x) (truncate x s)
                     (multiple-value-bind (py y) (truncate y s)
                       (and
                        (logbitp (+ px (* py r)) n)
                        (f (1- g) x y (/ s r))))))))
      (fresh-line stream)
      (dotimes (y s)
        (dotimes (x s)
          (princ
           (if (f g x y (/ s r))
               (format nil "~C " char)
               "  ") stream))
        (terpri stream)))))


;; Sierspinski Carpet
;; (fractal 495 3 3 :char #\black_small_square)

;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪ ▪ ▪       ▪ ▪ ▪ ▪ ▪ ▪       ▪ ▪ ▪ ▪ ▪ ▪       ▪ ▪ ▪
;; ▪   ▪       ▪   ▪ ▪   ▪       ▪   ▪ ▪   ▪       ▪   ▪
;; ▪ ▪ ▪       ▪ ▪ ▪ ▪ ▪ ▪       ▪ ▪ ▪ ▪ ▪ ▪       ▪ ▪ ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪                   ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪   ▪ ▪   ▪ ▪   ▪                   ▪   ▪ ▪   ▪ ▪   ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪                   ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪ ▪ ▪       ▪ ▪ ▪                   ▪ ▪ ▪       ▪ ▪ ▪
;; ▪   ▪       ▪   ▪                   ▪   ▪       ▪   ▪
;; ▪ ▪ ▪       ▪ ▪ ▪                   ▪ ▪ ▪       ▪ ▪ ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪                   ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪   ▪ ▪   ▪ ▪   ▪                   ▪   ▪ ▪   ▪ ▪   ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪                   ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪ ▪ ▪       ▪ ▪ ▪ ▪ ▪ ▪       ▪ ▪ ▪ ▪ ▪ ▪       ▪ ▪ ▪
;; ▪   ▪       ▪   ▪ ▪   ▪       ▪   ▪ ▪   ▪       ▪   ▪
;; ▪ ▪ ▪       ▪ ▪ ▪ ▪ ▪ ▪       ▪ ▪ ▪ ▪ ▪ ▪       ▪ ▪ ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪
;; ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪ ▪   ▪
;; ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪ ▪

;; Sierspinski's Triangle
;; (fractal 7 2 5 :char #\black_upper_left_triangle)

;; ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤
;; ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤
;; ◤ ◤     ◤ ◤     ◤ ◤     ◤ ◤     ◤ ◤     ◤ ◤     ◤ ◤     ◤ ◤
;; ◤       ◤       ◤       ◤       ◤       ◤       ◤       ◤
;; ◤ ◤ ◤ ◤         ◤ ◤ ◤ ◤         ◤ ◤ ◤ ◤         ◤ ◤ ◤ ◤
;; ◤   ◤           ◤   ◤           ◤   ◤           ◤   ◤
;; ◤ ◤             ◤ ◤             ◤ ◤             ◤ ◤
;; ◤               ◤               ◤               ◤
;; ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤                 ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤
;; ◤   ◤   ◤   ◤                   ◤   ◤   ◤   ◤
;; ◤ ◤     ◤ ◤                     ◤ ◤     ◤ ◤
;; ◤       ◤                       ◤       ◤
;; ◤ ◤ ◤ ◤                         ◤ ◤ ◤ ◤
;; ◤   ◤                           ◤   ◤
;; ◤ ◤                             ◤ ◤
;; ◤                               ◤
;; ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤
;; ◤   ◤   ◤   ◤   ◤   ◤   ◤   ◤
;; ◤ ◤     ◤ ◤     ◤ ◤     ◤ ◤
;; ◤       ◤       ◤       ◤
;; ◤ ◤ ◤ ◤         ◤ ◤ ◤ ◤
;; ◤   ◤           ◤   ◤
;; ◤ ◤             ◤ ◤
;; ◤               ◤
;; ◤ ◤ ◤ ◤ ◤ ◤ ◤ ◤
;; ◤   ◤   ◤   ◤
;; ◤ ◤     ◤ ◤
;; ◤       ◤
;; ◤ ◤ ◤ ◤
;; ◤   ◤
;; ◤ ◤
;; ◤


;; Vicsek fractal
;; (fractal 186 3 3 :char #\black_diamond)

;;                           ◆
;;                         ◆ ◆ ◆
;;                           ◆
;;                     ◆     ◆     ◆
;;                   ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆
;;                     ◆     ◆     ◆
;;                           ◆
;;                         ◆ ◆ ◆
;;                           ◆
;;         ◆                 ◆                 ◆
;;       ◆ ◆ ◆             ◆ ◆ ◆             ◆ ◆ ◆
;;         ◆                 ◆                 ◆
;;   ◆     ◆     ◆     ◆     ◆     ◆     ◆     ◆     ◆
;; ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆
;;   ◆     ◆     ◆     ◆     ◆     ◆     ◆     ◆     ◆
;;         ◆                 ◆                 ◆
;;       ◆ ◆ ◆             ◆ ◆ ◆             ◆ ◆ ◆
;;         ◆                 ◆                 ◆
;;                           ◆
;;                         ◆ ◆ ◆
;;                           ◆
;;                     ◆     ◆     ◆
;;                   ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆ ◆
;;                     ◆     ◆     ◆
;;                           ◆
;;                         ◆ ◆ ◆
;;                           ◆

