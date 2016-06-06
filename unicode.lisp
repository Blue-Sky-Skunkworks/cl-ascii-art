(in-package :cl-ascii-art)

(defparameter *sample-unicode-sets*
  `((:arrows                  #x2190 #x21ff)
    (:dingbats                #x2700 #x27FF)
    (:supplemental-arrows-a   #x27F0 #x27FF)
    (:supplemental-arrows-b   #x2900 #x297F)
    (:misc-symbols-and-arrows #x2B00 #x2B2F)
    (:mathematical-operators  #x2200 #x22FF)
    (:box-drawing             #x2500 #x257F)
    (:enclosed-alphanumerics  #x2460 #x24FE)
    (:block-elements          #x2580 #x259F)
    (:geometric-shapes        #x25A0 #x25FF)
    (:miscellaneous-symbols   #x2600 #x26FF)
    (:miscellaneous-technical #x2300 #x23FF)
    (:number-forms            #x2150 #x218F)
    (:i-ching-trigrams        #x2630 #x2637)
    (:i-ching-symbols         #x4DC0 #x4DFF)
    (:braille-patterns        #x2800 #x28FF)
    (:mahjong                 #x1F000 #x1F02B)
    (:dominos                 #x1F030 #x1F093)
    (:playing-cards           #x1F0A0 #x1F0F5)
    (:emoticons               #x1F601 #x1F64F)
    (:transportation          #x1F680 #x1F6F3)
    (:misc-3                  #x1F300 #x1F5FF)))

(defun show-unicode-characters (&optional which)
  "Show many sets of unicode arrows, dingbats, boxes, i-ching, etc."
  (if which
    (loop for set in (if (eq which :all) (mapcar #'car *sample-unicode-sets*) (ensure-list which))
          do (destructuring-bind (name start end) (or (assoc set *sample-unicode-sets*)
                                                      (error "unicode set ~A not found" set))
               (format t "~%~A~%~%" name)
               (loop for x from start to end
                     do (format t "~X : ~C   ~S~%" x (code-char x) (code-char x)))))
    (progn
      (format t "Please select a unicode set to view from the following.~%~%  :all~%")
      (loop for (name) in *sample-unicode-sets*
            do (format t "  ~(~S~)~%" name)))))

(defun unicode-apropos (search &key shuffle)
  (let* ((regex (create-scanner search))
         (list (iter (for index from 1 to #x1F6F3)
                 (let ((name (format nil "~S" (code-char index))))
                   (when (scan regex name)
                     (collect (code-char index)))))))
    (when shuffle (setf list (shuffle list)))
    (values list (coerce list 'string))))

(defparameter *ascii-to-braille-map*
  " A1B'K2L@CIF/MSP\"E3H9O6R^DJG>NTQ,*5<-U8V.%[$+X!&;:4\\0Z7(_?W]#Y)=")

(defun print-in-braille (string &optional (stream *standard-output*))
  (loop for character across string
        do (let ((pos (position (char-upcase character) *ascii-to-braille-map*)))
             (if pos
               (write-char (code-char (+ #x2800 pos)) stream)
               (warn "Character ~S is not a valid braille character." character)))))


(defparameter *clock-faces* (coerce (iter
                                      (with start = (char-code #\clock_face_one_oclock))
                                      (for c from start to (+ start 23))
                                      (collect (code-char c))) 'vector))

(defun clock-face (&optional (time (get-universal-time)))
  (multiple-value-bind (s m h) (decode-universal-time time )
    (declare (ignore s))
    (aref *clock-faces* (1- (+ (if (< m 30) 0 12) (if (> h 12) (- h 12) h))))))
