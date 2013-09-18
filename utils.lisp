(in-package :gk-trees)

(defvar *dbg-ids* nil
  "Identifiers used by DBG")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun set-debug (&rest ids)
  "Start dbg output on the given ids"
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on ids. With no ids, stop dbg altogether"
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-on-p (id)
  (member id *dbg-ids*))

(defun random-between (m n)
  "Returns a pseudo-random number between N (inclusive) and M (exclusive."
  (cond
    ((= m n)
     n)
    ((< n m)
     (+ (random (- m n)) n))
    ((> n m)
     (+ (random (- n m)) m))))

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

