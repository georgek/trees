(in-package :gk-trees)

(defparameter tg (make-gamma-graph (list (cord 'a 'b 2) (cord 'a 'c 1) (cord 'b 'c 2) (cord 'c 'd 3) (cord 'd 'e 1) (cord 'd 'f 3) (cord 'd 'g 8) (cord 'e 'f 2) (cord 'f 'h 6) (cord 'g 'h 3) (cord 'g 'i 4))))

(defparameter bg (make-gamma-graph (list (cord 'a 'b 2) (cord 'b 'c 4) (cord 'a 'e 6) (cord 'c 'e 6) (cord 'd 'e 2) (cord 'c 'f 8) (cord 'b 'g 8) (cord 'a 'h 12) (cord 'f 'h 12) (cord 'c 'i 12) (cord 'f 'i 12) (cord 'd 'j 12) (cord 'e 'j 12) (cord 'g 'j 12) (cord 'h 'j 10) (cord 'b 'k 12) (cord 'i 'k 10))))

(defparameter stg (make-graph (a b 3) (b c 2) (c d 1) (a c 2) (b d 1)))


(defun mess-up-cords (cords rem vary)
  "Messes up cords by removing the proportion REM and varying lengths by
  proportion VARY"
  (let* ((n (length cords))
         (nrem (round (* n rem))))
    (select-random cords (- n nrem))))

(defun mess-up-conn (cords rem vary)
  (loop for mcords = (mess-up-cords cords rem vary) do
       (when (= (length (components mcords)) 1)
         (return mcords))))

(defparameter n-tests 100)

(defun mean (list)
  (/ (reduce #'+ list) (length list)))

(defun random-test (degree leaves)
  (format t "ocords: ~d, oleaves: ~d~%~%" (/ (* leaves (1- leaves)) 2) leaves)
  (format t " mcords : nleaves :  ncords~%")
  (loop for i from 10 to 90 by 10 do
       (let (ocords
             (nleaves (list))
             (ncords (list)))
         (loop repeat n-tests
            for tree = (make-random-tree (range 1 leaves) degree)
            for cords = (tree-distances tree)
            for mcords = (mess-up-conn cords (/ i 100) nil)
            for l-tree = (ultrametric-lasso3 mcords)
            do
              (setf ocords (length mcords))
              (push (length (leafset l-tree)) nleaves)
              (push (length (lassoed-tree-used-cords l-tree)) ncords))
         (format t "~7d : ~7,2f : ~7,2f~%"
                 ocords
                 (mean nleaves)
                 (mean ncords)))))

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

(defun csv-to-cords (filename &key (limit most-positive-fixnum)
                                (delimiter #\,))
  "Makes all (unique) cords from a CSV matrix file."
  (let ((cords (list)))
   (with-open-file (filein filename)
     (loop for line = (read-line filein nil)
        for i from 1 to limit
        while line do
          (loop for distance in (split-string line delimiter)
             for j from 1 to (1- i) do
               (push (cord i j (read-from-string distance)) cords))))
   cords))

