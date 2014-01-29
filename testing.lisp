(in-package :gk-trees)

(defun print-tree-and-cords (tree &optional (stream t))
  (let ((used-cords (lassoed-tree-used-cords tree)))
    (pp-tree-print tree stream)
    (format stream "Cords (~A): ~A~%" (length used-cords) used-cords)))

(defun add-noise-to-cords (cords amount)
  "Adds noise to the lengths of the cords.  AMOUNT controls the amount of
  noise.  A value of 1 means a distance can be varied by as much as the value
  of the mean length."
  (let ((amt (* amount (floor (mean (mapcar #'cord-length cords))))))
   (mapcar (lambda (c) (cord (cord-left c) (cord-right c)
                             (max
                              (+ (cord-length c) (random-between (- amt) amt))
                              0)))
           cords)))

(defun mess-up-cords (cords rem &optional (vary 0))
  "Messes up cords by removing the proportion REM and varying lengths by
  proportion VARY"
  (let* ((n (length cords))
         (nrem (round (* n rem))))
    (add-noise-to-cords (select-random cords (- n nrem)) vary)))

(defun mess-up-conn (cords rem &optional (vary 0))
  (loop for mcords = (mess-up-cords cords rem vary) do
       (when (= (length (components mcords)) 1)
         (return mcords))))

(defparameter n-tests 100)

(defun random-test (degree leaves)
  (let ((ncords (/ (* leaves (1- leaves)) 2)))
    ;; (format t "ncords: ~d, nleaves: ~d~%~%" ncords leaves)
    (format t " mcords  nleaves   ncords~%")
    (loop for i from 10 to 90 by 10 do
         (let (nmcords
               (nleaves (list))
               (recovered-cords (list)))
           (loop repeat n-tests
              for tree = (make-random-tree (range 1 leaves) degree)
              for cords = (tree-distances tree)
              for mcords = (mess-up-conn cords (/ i 100) 0)
              for l-tree = (ultrametric-lasso3 mcords)
              do
                (setf nmcords (length mcords))
                (push (length (leafset l-tree)) nleaves)
                (push (length (lassoed-tree-used-cords l-tree)) recovered-cords))
           (format t "~7,2f  ~7,2f  ~7,2f~%"
                   (/ nmcords ncords)
                   (/ (mean nleaves) leaves)
                   (/ (mean recovered-cords) nmcords))))))

(defun random-tests-pgfplots (nleaves degrees)
  "DEGREES is a list of degrees to run the experiment on."
  (loop for degree in degrees do
       (format t "      \\addplot table[x=mcords,y=ncords] {~%")
       (random-test degree nleaves)
       (format t "   };~%   \\addlegendentry{~d}~%" degree)))

(defun file-string (filename)
  (with-open-file (filein filename)
    (reduce (lambda (s1 s2) (concatenate 'string s1 s2))
            (loop for line = (read-line filein nil)
               while line collect line))))

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
                                (delimiter #\,) (labelled nil)
                                (truncate-labels nil))
  "Makes all (unique) cords from a CSV matrix file."
  (let ((cords (list))
        (names (list))
        (namefun #'identity))
   (with-open-file (filein filename)
     (when labelled
       (setf names (split-string (read-line filein nil) delimiter))
       (setf namefun (lambda (n) (nth (1- n) names))))
     (when truncate-labels
       (setf names (mapcar (lambda (s)
                             (subseq s 0 (min (length s) truncate-labels)))
                           names)))
     (loop for line = (read-line filein nil)
        for i from 1 to limit
        while line do
          (loop for distance in (split-string line delimiter)
             for j from 1 to (1- i) do
               (unless (string= distance "?")
                (push (cord (funcall namefun i)
                            (funcall namefun j)
                            (read-from-string distance))
                      cords)))))
   cords))

(defun csv-test-pgfplots (filename)
  (let* ((cords (csv-to-cords filename))
         (ncords (length cords))
         (leaves (length (cords-vertices cords))))
    (format t " mcords  nleaves   ncords~%")
    (loop for i from 10 to 90 by 10 do
         (let (nmcords
               (nleaves (list))
               (recovered-cords (list)))
           (loop repeat n-tests
              for mcords = (mess-up-conn cords (/ i 100) nil)
              for l-tree = (ultrametric-lasso3 mcords)
              do
                (setf nmcords (length mcords))
                (push (length (leafset l-tree)) nleaves)
                (push (length (lassoed-tree-used-cords l-tree)) recovered-cords))
           (format t "~7,2f  ~7,2f  ~7,2f~%"
                   (/ nmcords ncords)
                   (/ (mean nleaves) leaves)
                   (/ (mean recovered-cords) nmcords))))))

(defun noise-test (degree leaves)
  (loop for i from 0 to 3 by 0.1
     for distances = (list)
     do
       (loop repeat n-tests
          for otree = (make-random-tree (range 1 leaves) degree)
          for ctree = (ultrametric-lasso3 (mess-up-conn (tree-distances otree)
                                                        0 i))
          do (push (tree-distance otree ctree) distances))
       (format t "~7,2f ~7,2f~%" i (mean distances))))

