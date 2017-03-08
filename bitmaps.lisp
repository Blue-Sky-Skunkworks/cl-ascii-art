(in-package :cl-ascii-art)

(defun make-bitmap (width height)
  (make-array (list height width) :initial-element nil))

(defvar *bitmap*)

(defmacro with-bitmap ((width height) &body body)
  `(let ((*bitmap* (make-bitmap ,width ,height)))
     ,@body))

(defun outside-bounds (x y &optional (bitmap *bitmap*))
  (destructuring-bind (height width) (array-dimensions bitmap)
    (or (< x 0) (< y 0) (>= x width) (>= y height))))

(defun set-pixel (x y &optional (bitmap *bitmap*) (value t))
  (unless (outside-bounds x y bitmap)
    (setf (aref bitmap y x) value)))

(defun draw (&key (stream *standard-output*) (bitmap *bitmap*))
  (destructuring-bind (height width) (array-dimensions bitmap)
    (iter (for y from 0 to (1- height) by 2)
      (iter (for x from 0 to (1- width))
        (princ
         (let ((top (aref bitmap y x))
               (bottom (when (< y (1- height)) (aref bitmap (1+ y) x))))
           (cond
             ((and top bottom) #\FULL_BLOCK)
             (top              #\UPPER_HALF_BLOCK)
             (bottom           #\LOWER_HALF_BLOCK )
             (t                #\SPACE)))
         stream))
      (fresh-line stream))))

(defun pattern-to-bitmap (pattern)
  (let ((bitmap (make-bitmap (length (car pattern)) (length pattern))))
    (iter (for row in pattern)
      (for y from 0)
      (iter (for character in-vector row)
        (for x from 0)
         (when (not (char= character #\space))
           (setf (aref bitmap y x) t))))
    bitmap))

(defun copy-bitmap-onto-bitmap (from-bitmap to-bitmap x y &key (fn (lambda (a b) (or a b))))
  (destructuring-bind (height width) (array-dimensions from-bitmap)
    (iter (for yi from 0 to (1- height))
      (iter (for xi from 0 to (1- width))
        (let ((from (aref from-bitmap yi xi))
              (to (aref to-bitmap (+ y yi) (+ x xi))))
          (set-pixel (+ x xi) (+ y yi)
                     to-bitmap
                     (funcall fn from to)))))))

;;; Described in
;;; Computer Graphics - Principles and Practice by Donald Hearn and M. Pauline Baker

(defun draw-circle (x-center y-center radius &optional (bitmap *bitmap*))
  (labels ((pixel (x y) (set-pixel (+ x-center x) (+ y-center y) bitmap))
           (draw-points (x y)
             (pixel x     y)
             (pixel (- x) y)
             (pixel x     (- y))
             (pixel (- x) (- y))
             (pixel y     x)
             (pixel (- y) x)
             (pixel y     (- x))
             (pixel (- y) (- x))))
    (loop with x = 0
          with y = radius
          with p = (- 1 radius)
          initially (draw-points x y)
          while (< x y)
          do (incf x)
             (if (< p 0)
               (incf p (+ (* 2 x) 1))
               (progn
                 (decf y)
                 (incf p (+ (* 2 (- x y)) 1))))
             (draw-points x y))))

(defun draw-line (xa ya xb yb &optional (bitmap *bitmap*))
  (let* ((dx (- xb xa))
         (dy (- yb ya))
         (steps (if (> (abs dx) (abs dy)) (abs dx) (abs dy)))
         (xi (/ dx steps))
         (yi (/ dy steps)))
    (set-pixel xa ya bitmap)
    (loop with x = xa
          with y = ya
          for k from 0 to (1- steps)
          do (incf x xi)
             (incf y yi)
             (set-pixel (floor x) (floor y) bitmap))))

(defun fill-bitmap (x y &optional (bitmap *bitmap*))
  (unless (outside-bounds x y bitmap)
    (unless (aref bitmap y x)
      (setf (aref bitmap y x) t)
      (fill-bitmap (+ x 1) y bitmap)
      (fill-bitmap (- x 1) y bitmap)
      (fill-bitmap x (+ y 1) bitmap)
      (fill-bitmap x (- y 1) bitmap))))

(defun draw-filled-circle (x-center y-center radius &optional (bitmap *bitmap*))
  (draw-circle x-center y-center radius bitmap)
  (fill-bitmap x-center y-center bitmap))



;;;        ╺┳┓┏━╸┏┳┓┏━┓┏━┓╻
;;;         ┃┃┣╸ ┃┃┃┃ ┃┗━┓╹
;;;        ╺┻┛┗━╸╹ ╹┗━┛┗━┛╹


;; █      ▄▀      ▄▀      ▄▀     ▄▀
;; █      █      █      ▄▀     ▄▀
;; █     █      █      █     ▄▀
;; █     █     █     ▄▀    ▄▀
;; █    █     █     █    ▄▀     ▄▀▀
;; █    █    █    ▄▀   ▄▀    ▄▀▀
;; █   █    █   ▄▀   ▄▀   ▄▄▀
;; █   █   █   ▄▀  ▄▀   ▄▀
;; █  █   █  ▄▀  ▄▀  ▄▀▀      ▄▄▀▀▀
;; █  █  █  ▄▀ ▄▀ ▄▄▀     ▄▄▀▀
;; █ █  █ ▄▀ ▄▀ ▄▀    ▄▄▀▀
;; █ █ █ █ ▄▀▄▀▀  ▄▄▀▀
;; ██ █▄▀▄█▄▀ ▄▄▀▀        ▄▄▄▄▀▀▀▀▀
;; ████▄█▀▄▄▀▀   ▄▄▄▄▀▀▀▀▀
;; ███████▄▄▀▀▀▀▀
;; █████▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄


(defun sunbeam (&key (step 8) (size 64))
  "Draw a sunbeam."
  (with-bitmap (size size)
    (iter (for x from 0 to size by step)
      (draw-line 0 (1- size) x 0)
      (draw-line 0 (1- size) (1- size) x))
    (draw)))



;;             ▄▄▄▄▄▄▄
;;        ▄▄█████████████▄▄
;;      ▄███████████████████▄
;;    ▄███████████████████████▄
;;   ███████████████████████████
;;  █████████████████████████████
;; ▄█████████████████████████████▄
;; ███████████████████████████████
;; ███████████████████████████████
;; ███████████████████████████████
;;  █████████████████████████████
;;  ▀███████████████████████████▀
;;   ▀█████████████████████████▀
;;     ▀█████████████████████▀
;;       ▀█████████████████▀
;;          ▀▀▀███████▀▀▀

(defun sun (&key (size 64))
  "Draw a sun."
  (with-bitmap (size size)
    (let ((mid (floor size 2)))
      (draw-filled-circle mid mid (1- mid))
      (draw))))


;;         ▄▄▄▀▀▀▀▀▀▀▄▄▄
;;      ▄▄▀             ▀▄▄
;;    ▄▀    ▄▄▀▀▀▀▀▀▀▄▄    ▀▄
;;   ▄▀   ▄▀           ▀▄   ▀▄
;;  █   ▄▀    ▄▀▀▀▀▀▄    ▀▄   █
;; ▄▀  ▄▀   ▄▀       ▀▄   ▀▄  ▀▄
;; █   █   █   ▄▀▀▀▄   █   █   █
;; █   █   █   █   █   █   █   █
;; █   █   ▀▄   ▀▀▀   ▄▀   █   █
;;  █   █    ▀▄     ▄▀    █   █
;;  ▀▄   ▀▄    ▀▀▀▀▀    ▄▀   ▄▀
;;    █    ▀▄▄       ▄▄▀    █
;;     ▀▄▄    ▀▀▀▀▀▀▀    ▄▄▀
;;        ▀▄▄▄       ▄▄▄▀
;;            ▀▀▀▀▀▀▀


(defun bullseye (&key (size 64) (step 4) filled (draw t))
  "Draw a bullseye."
  (with-bitmap (size size)
    (let ((mid (floor size 2)))
      (loop for radius from 2 to mid by step
            do (draw-circle mid mid radius))
      (when filled
        (loop for x from 2 to mid by (* 2 step)
              do (fill-bitmap (+ mid x 1) mid)))
      (if draw
        (draw)
        *bitmap*))))


;;  ▄▀▀▀▀▄
;; █ ▀  ▀ █
;; █ ▀▄▄▀ █
;;  ▀▄▄▄▄▀

(defun smile ()
  "When you're smiling, the whole world smiles with you."
  (draw :bitmap (pattern-to-bitmap '("  ****  "
                                     " *    * "
                                     "* *  * *"
                                     "*      *"
                                     "* *  * *"
                                     "*  **  *"
                                     " *    * "
                                     "  ****  "))))

 ;;    ▄▄▄▄▄
 ;;  ▄▀  █  ▀▄
 ;; █    █    █
 ;; █   ▄█▄   █
 ;; ▀▄▄▀ █ ▀▄▄▀
 ;;   ▀▄▄█▄▄▀


(defun peace ()
  "Peace on Earth."
  (with-bitmap (12 12)
    (draw-circle 6 6 5)
    (draw-line 6 10 6 1)
    (draw-line 6 6 3 9)
    (draw-line 6 6 9 9)
    (draw)))
