(defparameter tg (make-gamma-graph (list (cord 'a 'b 2) (cord 'a 'c 1) (cord 'b 'c 2) (cord 'c 'd 3) (cord 'd 'e 1) (cord 'd 'f 3) (cord 'd 'g 8) (cord 'e 'f 2) (cord 'f 'h 6) (cord 'g 'h 3) (cord 'g 'i 4))))

(defparameter bg (make-gamma-graph (list (cord 'a 'b 2) (cord 'b 'c 4) (cord 'a 'e 6) (cord 'c 'e 6) (cord 'd 'e 2) (cord 'c 'f 8) (cord 'b 'g 8) (cord 'a 'h 12) (cord 'f 'h 12) (cord 'c 'i 12) (cord 'f 'i 12) (cord 'd 'j 12) (cord 'e 'j 12) (cord 'g 'j 12) (cord 'h 'j 10) (cord 'b 'k 12) (cord 'i 'k 10))))

(defparameter stg (make-graph (a b 3) (b c 2) (c d 1) (a c 2) (b d 1)))

(defun select-random (list amount)
  "Returns AMOUNT random elements from LIST"
  (let ((result (list))
        (n (length list)))
    (loop for item in list do
         (when (< (random n) amount)    ; chance amount/n
           (setf result (cons item result))
           (decf amount))
         (decf n))
    (nreverse result)))

(defun mess-up-cords (cords rem vary)
  "Messes up cords by removing the proportion REM and varying lengths by
  proportion VARY"
  (let* ((n (length cords))
         (nrem (round (* n rem))))
    (select-random cords (- n nrem))))

