(in-package :gk-trees)

(defvar *dbg-ids* nil
  "Identifiers used by DBG")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dbgo (object)
  (format *debug-io* "~A~%" object)
  object)

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

(defun positions (items sequence &key (test #'eql))
  (mapcar (lambda (item) (position item sequence :test test)) items))

(defun split-string (string delimiter &key (omit-nulls t))
  (assert (stringp string))
  (assert (characterp delimiter))
  (let ((splits (list)))
   (loop for pos = (position delimiter string)
      while pos do
        (push (subseq string 0 pos) splits)
        (setf string (subseq string (1+ pos)))
      finally (push string splits))
   (when omit-nulls
     (setf splits (delete "" splits :test #'equal)))
   (nreverse splits)))

(defun file-string (filename)
  (with-open-file (filein filename)
    (reduce (lambda (s1 s2) (concatenate 'string s1 s2))
            (loop for line = (read-line filein nil)
               while line collect line))))

(defun html-colour (string)
  (check-type string string)
  (assert (= (length string) 6))
  (list (parse-integer string :start 0 :end 2 :radix 16)
        (parse-integer string :start 2 :end 4 :radix 16)
        (parse-integer string :start 4 :end 6 :radix 16)))

(defmacro defmap (a b &body values)
  (let ((mapnames (list (intern (format nil "~A2~A" (symbol-name a) (symbol-name b)))
                        (intern (format nil "~A2~A" (symbol-name b) (symbol-name a)))))
        (valsym (gensym "values")))
    `(let ((,valsym ',values))
       (progn
         ,@(loop for mapname in mapnames append
                `((defvar ,mapname (make-hash-table :test #'equal))
                  (loop with val = ,valsym
                     while (consp val) do
                       (setf (gethash (car val) ,mapname) (cadr val))
                       (setf val (cddr val)))))))))

