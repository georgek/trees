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
  "Messes up cords by removing REM from the implied matrix and varying lengths
  by proportion VARY"
  (let* ((nleaves (length (remove-duplicates
                           (append (mapcar #'cord-left cords)
                                   (mapcar #'cord-right cords)))))
         (whole-matrix (append cords cords
                               (make-list nleaves :initial-element :identity)))
         (n (length whole-matrix))
         (nrem (round (* n rem)))
         (rem-matrix (select-random whole-matrix (- n nrem))))
    (add-noise-to-cords
     (remove-duplicates (remove :identity rem-matrix :test #'eq) :test #'eq)
     vary)))

(defun mess-up-conn (cords rem &optional (vary 0))
  (loop for mcords = (mess-up-cords cords rem vary) do
       (when (= (length (components mcords)) 1)
         (return mcords))))

(defparameter n-tests 500)

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

(defun csv-test-pgfplots (filename &key (limit most-positive-fixnum)
                                     (delimiter #\,) (labelled nil)
                                     (truncate-labels nil))
  (let* ((cords (csv-to-cords filename :limit limit :delimiter delimiter :labelled labelled
                              :truncate-labels truncate-labels))
         (ncords (length cords))
         (leaves (length (cords-vertices cords))))
    (format t " mcords  nleaves   ncords~%")
    (loop for i from 10 to 90 by 10 do
         (let (nmcords
               (nleaves (list))
               (recovered-cords (list)))
           (loop repeat n-tests
              for mcords = (mess-up-conn cords (/ i 100))
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

;;; this test measures the robinson-foulds between trees built from partial
;;; distances and the tree built from the complete distance
(defun partial-rob-foulds (cords)
  (let* ((comp-tree (ultrametric-lasso3 cords))
         (max-rf (* 2 (1- (length (leafset comp-tree))))))
    (format t "missing  mean      min      max     minc     maxc~%")
    (loop for rem in (list 0 1 5 10 20 30)
       for dists = (list)
       for ncords = (list)
       do
         (loop repeat n-tests
            for ntree = (ultrametric-lasso3 (mess-up-conn cords (/ rem 100)))
            do
              (push (tree-distance comp-tree ntree) dists)
              (push (length (leafset ntree)) ncords))
         (format t "~f  ~7,3f  ~7,3f  ~7,3f  ~7d  ~7d~%"
                 rem
                 (/ (mean dists) max-rf)
                 (/ (reduce #'min dists) max-rf)
                 (/ (reduce #'max dists) max-rf)
                 (reduce #'min ncords)
                 (reduce #'max ncords)))))

;;; this test how many times a given cluster is present in a constructed tree
(defun partial-cluster (cords rem cluster)
  (let ((times-present 0.0))
    (loop repeat n-tests
       for tree = (ultrametric-lasso3 (mess-up-conn cords rem))
       do
         (when (member cluster (tree-clusters tree)
                       :test #'sets-equal)
           (incf times-present)))
    (format t "Present: ~f%~%" (* (/ times-present n-tests) 100))))

;;; this tests how many leaves a supertree has when made from two smaller
;;; matrices, TREE-SIZE is the size of the leafsets of the subtrees,
;;; OVERLAP-SIZE is the number leaves they have in common
(defun supertree-test (tree-size overlap-size &optional (degree 2))
  (assert (> tree-size overlap-size))
  (let* ((nleaves (- (* 2 tree-size) overlap-size))
         (supertree (make-random-tree (range 1 nleaves) degree))
         (subtree1 (tree-subtree supertree (range 1 tree-size)))
         (subtree2 (tree-subtree supertree (range (1+ (- tree-size overlap-size))
                                                  nleaves))))
    (setf supertree (ultrametric-lasso3 (union (tree-distances subtree1)
                                               (tree-distances subtree2)
                                               :test #'cords-equal)))
    (when (and (dbg-on-p :supertree-test)
               (<= (length (leafset supertree)) 5))
      (pp-tree-print supertree))
    (length (leafset supertree))))

(defun repeat-test (function)
  "Repeatedly call a function to get a min, max and mean value."
  (loop repeat n-tests
     for value = (funcall function)
     collecting value into values
     finally
       (return (list (reduce #'min values) (reduce #'max values) (mean values)))))

(defun supertree-overlap-test (tree-size max-overlap &optional (degree 2))
  (assert (> tree-size max-overlap))
  (format t "overlap  min       max      mean~%")
  (loop for overlap-size from 1 to max-overlap
     for sizes = (repeat-test (lambda ()
                                (supertree-test tree-size overlap-size degree)))
     for percentages = (mapcar (lambda (s) (/ s (- (* 2 tree-size) overlap-size))) sizes)
     do
       (format t "~,3d    ~7,2f   ~7,2f   ~7,2f~%"
               overlap-size (first percentages) (second percentages) (third percentages))))

(defun format-single (values)
  "VALUES should be a list with min, max and mean."
  (format t "min:  ~7,2f~%max:  ~7,2f~%mean: ~7,2f~%"
          (first values) (second values) (third values)))

