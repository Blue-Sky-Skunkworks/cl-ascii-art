(in-package :cl-ascii-art)

(defun clock-face (&optional (time (get-universal-time)))
  (multiple-value-bind (s m h) (decode-universal-time time )
    (declare (ignore s))
    (aref
     #(#\CLOCK_FACE_ONE_OCLOCK
       #\CLOCK_FACE_TWO_OCLOCK
       #\CLOCK_FACE_THREE_OCLOCK
       #\CLOCK_FACE_FOUR_OCLOCK
       #\CLOCK_FACE_FIVE_OCLOCK
       #\CLOCK_FACE_SIX_OCLOCK
       #\CLOCK_FACE_SEVEN_OCLOCK
       #\CLOCK_FACE_EIGHT_OCLOCK
       #\CLOCK_FACE_NINE_OCLOCK
       #\CLOCK_FACE_TEN_OCLOCK
       #\CLOCK_FACE_ELEVEN_OCLOCK
       #\CLOCK_FACE_TWELVE_OCLOCK
       #\CLOCK_FACE_ONE-THIRTY
       #\CLOCK_FACE_TWO-THIRTY
       #\CLOCK_FACE_THREE-THIRTY
       #\CLOCK_FACE_FOUR-THIRTY
       #\CLOCK_FACE_FIVE-THIRTY
       #\CLOCK_FACE_SIX-THIRTY
       #\CLOCK_FACE_SEVEN-THIRTY
       #\CLOCK_FACE_EIGHT-THIRTY
       #\CLOCK_FACE_NINE-THIRTY
       #\CLOCK_FACE_TEN-THIRTY
       #\CLOCK_FACE_ELEVEN-THIRTY
       #\CLOCK_FACE_TWELVE-THIRTY)
     (1- (+ (if (< m 30) 0 12) (if (> h 12) (- h 12) h))))))

