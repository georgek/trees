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
  (let* ((nleaves (length (cords-vertices cords)))
         (whole-matrix (append cords cords
                               (make-list nleaves :initial-element :identity)))
         (n (length whole-matrix))
         (nrem (round (* n rem)))
         (rem-matrix (select-random whole-matrix (- n nrem))))
    (add-noise-to-cords
     (remove-duplicates (remove :identity rem-matrix :test #'eq) :test #'eq)
     vary)))

(defun mess-up-cords2 (cords rem &optional (vary 0))
  "Messes up cords by removing the proportion REM and varying lengths by
  proportion VARY"
  (let* ((n (length cords))
         (nrem (round (* n rem))))
    (add-noise-to-cords (select-random cords (- n nrem)) vary)))

(defun mess-up-conn (cords rem &optional (vary 0))
  (loop for mcords = (mess-up-cords cords rem vary) do
       (when (= (length (components mcords)) 1)
         (return mcords))))

(defun mess-up-conn2 (cords rem &optional (vary 0))
  (loop for mcords = (mess-up-cords2 cords rem vary) do
       (when (= (length (components mcords)) 1)
         (return mcords))))

(defun many-messed-up-cords (n cords rem &optional (vary 0))
  (loop repeat n collecting
       (mess-up-conn cords rem vary)))

(defun many-messed-up-cords2 (n cords rem &optional (vary 0))
  (loop repeat n collecting
       (mess-up-conn2 cords rem vary)))

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
                   i
                   (* (/ (mean nleaves) leaves) 100)
                   (* (/ (mean recovered-cords) nmcords) 100))))))

(defun random-tests-pgfplots (nleaves degrees)
  "DEGREES is a list of degrees to run the experiment on."
  (loop for degree in degrees do
       (format t "      \\addplot table[x=mcords,y=ncords] {~%")
       (random-test degree nleaves)
       (format t "   };~%   \\addlegendentry{~d}~%" degree)))

(defun csv-to-cords (filename &key (limit most-positive-fixnum)
                                (delimiter #\,) (labelled t)
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
(defun partial-rob-foulds (tree)
  (let* ((cords (tree-distances tree))
         (max-rf (* 2 (1- (length (leafset tree))))))
    (format t "missing  mean      min      max     minc     maxc    meanc~%")
    (loop for rem in (list 0 1 5 10 20 30)
       for dists = (list)
       for ncords = (list)
       do
         (loop repeat n-tests
            for ntree = (ultrametric-lasso3 (mess-up-conn cords (/ rem 100)))
            do
              (push (tree-distance tree ntree) dists)
              (push (length (lassoed-tree-used-cords ntree)) ncords))
         (format t "~f  ~7,3f  ~7,3f  ~7,3f  ~7d  ~7d  ~7,3f~%"
                 rem
                 (/ (mean dists) max-rf)
                 (/ (reduce #'min dists) max-rf)
                 (/ (reduce #'max dists) max-rf)
                 (reduce #'min ncords)
                 (reduce #'max ncords)
                 (mean ncords)))))

(defun partial-rob-foulds-cords (cords amounts)
  (let* ((tree (ultrametric-lasso3 cords))
         (max-rf (* 2 (1- (length (leafset tree))))))
    (format t "missing  mean      min      max     minc     maxc    meanc~%")
    (loop for rem in amounts
       for dists = (list)
       for ncords = (list)
       do
         (loop repeat n-tests
            for ntree = (ultrametric-lasso3 (mess-up-conn2 cords (/ rem 100)))
            do
              (push (tree-distance tree ntree) dists)
              (push (length (lassoed-tree-used-cords ntree)) ncords))
         (format t "~f  ~7,3f  ~7,3f  ~7,3f  ~7d  ~7d  ~7,3f~%"
                 rem
                 (/ (mean dists) max-rf)
                 (/ (reduce #'min dists) max-rf)
                 (/ (reduce #'max dists) max-rf)
                 (reduce #'min ncords)
                 (reduce #'max ncords)
                 (mean ncords)))))

(defun noisy-rob-foulds (tree)
  (let* ((cords (tree-distances tree))
         (max-rf (* 2 (1- (length (leafset tree))))))
    (format t "noise   mean      min      max     minc     maxc    meanc~%")
    (loop for noise in (list 0 1 5 10 20 30)
       for dists = (list)
       for ncords = (list)
       do
         (loop repeat n-tests
            for ntree = (ultrametric-lasso3 (mess-up-conn cords 0 (float (/ noise 100))))
            do
              (push (tree-distance tree ntree) dists)
              (push (length (leafset ntree)) ncords))
         (format t "~f  ~7,3f  ~7,3f  ~7,3f  ~7d  ~7d  ~7,3f~%"
                 noise
                 (/ (mean dists) max-rf)
                 (/ (reduce #'min dists) max-rf)
                 (/ (reduce #'max dists) max-rf)
                 (reduce #'min ncords)
                 (reduce #'max ncords)
                 (mean ncords)))))

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

;;; combines two sets of cords across the same pairs of vertices into one set
;;; by taking the mean of the distance for each pair of cords, pairs of cords
;;; where the ratio between the distances is too high are removed
(defun combine-cords (cords1 cords2 &key (remove-outliers t))
  (let* ((factors (mapcar #'/
                          (mapcar #'cord-length cords1)
                          (mapcar #'cord-length cords2)))
         (hq (high-quartile factors))
         (lq (low-quartile factors))
         (iqr (- hq lq))
         (comb-cords (list)))
    (loop
       for cord1 in cords1
       for cord2 in cords2
       for factor in factors do
         (format t "~A~%~A~%" cord1 cord2)
         (assert (cords-equal cord1 cord2 #'equal))
         (unless (and remove-outliers
                      (or (> factor (+ hq iqr))
                          (< factor (- lq iqr))))
           (push (cord (cord-left cord1) (cord-right cord1)
                       (/ (+ (cord-length cord1) (cord-length cord2)) 2))
                 comb-cords)))
    comb-cords))

(defun csv-to-alist (filename)
  (let ((alist (list)))
    (with-open-file (filein filename)
      (loop for line = (read-line filein nil)
         while line do
           (push (reduce #'cons (split-string line #\,)) alist)))
    (nreverse alist)))

(defun argmax (list)
  (loop for idx from 0
     for val in list
     with maxval = 0
     with maxidx = 0
     do
       (when (> val maxval)
         (setf maxval val)
         (setf maxidx idx))
     finally (return maxidx)))

(defun q-matrix-to-groups (filename group-names)
  (let ((alist (list)))
    (with-open-file (filein filename)
      (loop for line = (read-line filein nil)
         while line
         for name in group-names
         for splits = (split-string line #\Space) do
           (push (cons name (1+ (argmax (mapcar #'read-from-string splits)))) alist)))
    (nreverse alist)))

(defun mix-colours (amounts colours)
  (let ((sum (reduce #'+ amounts)))
    (mapcar (lambda (c) (min (round (/ c sum)) 255))
            (reduce (lambda (c1 c2) (mapcar #'+ c1 c2))
                    (mapcar (lambda (a c) (mapcar (lambda (c) (* c a)) c))
                            amounts colours)))))

(defun q-matrix-to-colour-map (filename group-names colours)
  (let ((colourmap (list)))
    (with-open-file (filein filename)
      (loop for line = (read-line filein nil)
         while line
         for name in group-names
         for amounts = (mapcar #'read-from-string
                               (split-string line #\Space))
         do
           (push (cons name (mix-colours amounts colours)) colourmap)))
    (nreverse colourmap)))

(defun two-matrix-tree (mat1 mat2 overlap1 overlap2)
  "Builds tree by combining mat1 and mat2.  overlap1 and overlap2 should be
  lists of equal length which are subsets of (names mat1) and (names mat2)
  respectively indicating which elements are identical between the matrices."
  (assert (= (length overlap1) (length overlap2)))
  (let* ((cords1 (matrix-to-cords mat1))
         (cords2 (matrix-to-cords mat2))
         overlap-cords all-cords)
    (dbg :2mat "Making cords1...~%")
    (multiple-value-bind (cords1 overlap-cords1)
        (b-remove-if (lambda (c) (and (find (cord-left c) overlap1 :test #'equal)
                                      (find (cord-right c) overlap1 :test #'equal)))
                     cords1)
      (dbg :2mat "ol1: ~D~%" (length overlap-cords1))
      (dbg :2mat "Makign cords2...~%")
      (multiple-value-bind (cords2 overlap-cords2)
          (b-remove-if (lambda (c) (and (find (cord-left c) overlap2 :test #'equal)
                                        (find (cord-right c) overlap2 :test #'equal)))
                       cords2)
        (dbg :2mat "ol2: ~D~%" (length overlap-cords2))
        (dbg :2mat "Making overlap...~%")
        (setf overlap-cords
              (combine-cords
               (mapcar (lambda (c) (cord (format nil "C~D"
                                                 (1+ (position (cord-left c) overlap1 :test #'equal)))
                                         (format nil "C~D"
                                                 (1+ (position (cord-right c) overlap1 :test #'equal)))
                                         (cord-length c)))
                       overlap-cords1)
               (mapcar (lambda (c) (cord (format nil "C~D"
                                                 (1+ (position (cord-left c) overlap2 :test #'equal)))
                                         (format nil "C~D"
                                                 (1+ (position (cord-right c) overlap2 :test #'equal)))
                                         (cord-length c)))
                       overlap-cords2)))
        (dbg :2mat "Making all-cords...~%")
        (setf all-cords (union (mapcar (lambda (c) (cord (format nil "A~A" (cord-left c))
                                                         (format nil "A~A" (cord-right c))
                                                         (cord-length c)))
                                       cords1)
                               (mapcar (lambda (c) (cord (format nil "B~A" (cord-left c))
                                                         (format nil "B~A" (cord-right c))
                                                         (cord-length c)))
                                       cords2)
                               :test #'cords-equal))
        (setf all-cords (union all-cords overlap-cords :test #'cords-equal))
        (dbg :2mat "Checking...~%")
        (assert (= (+ (length cords1) (length cords2) (length overlap-cords))
                   (length all-cords)))
        
          )
        )
    )
  )

(defun label-to-biglabel-function (overlap unique-prefix overlap-names)
  (lambda (label)
   (let ((pos (position label overlap :test #'equal)))
     (if pos
         (nth pos overlap-names)
         (format nil "~A~A" unique-prefix label)))))

(defun cord-overlap-p (cord overlap-prefix-char)
  (declare (type character overlap-prefix-char))
  (and (char= (aref (cord-left cord) 0) overlap-prefix-char)
       (char= (aref (cord-right cord) 0) overlap-prefix-char)))

(defun cord< (cord1 cord2)
  (or (string< (cord-left cord1) (cord-left cord2))
      (string< (cord-right cord1) (cord-right cord2))))

(defun overlap-labels (n prefix)
  (mapcar (lambda (i) (format nil "~A~A" prefix i)) (range 1 n)))

(defun two-matrix-tree2 (mat1 mat2 overlap1 overlap2 &key (remove-outliers t))
  (assert (= (length overlap1) (length overlap2)))
  (let ((mat1 (copy-matrix mat1))
        (mat2 (copy-matrix mat2))
        (overlap-labels (overlap-labels (length overlap1) "C"))
        cords1 cords2
        overlap-cords)
    (setf (names mat1) (mapcar (label-to-biglabel-function overlap1 "A" overlap-labels)
                               (names mat1)))
    (setf (names mat2) (mapcar (label-to-biglabel-function overlap2 "B" overlap-labels)
                               (names mat2)))
    (dbg :2mat "names1: ~A~%" (names mat1))
    (dbg :2mat "names2: ~A~%" (names mat2))
    (dbg :2mat "Making cords...")
    (setf cords1 (matrix-to-cords mat1))
    (setf cords2 (matrix-to-cords mat2))
    (dbg :2mat "Removing overlap1...")
    (multiple-value-bind (cords1 overlap-cords1)
        (b-remove-if (lambda (c) (cord-overlap-p c #\C)) cords1)
      (dbg :2mat "Removing overlap2...")
      (multiple-value-bind (cords2 overlap-cords2)
          (b-remove-if (lambda (c) (cord-overlap-p c #\C)) cords2)
        (setf overlap-cords1 (sort overlap-cords1 #'cord<))
        (setf overlap-cords2 (sort overlap-cords2 #'cord<))
        (dbg :2mat "Combining overlap cords (~D)..." (length overlap-cords1))
        (setf overlap-cords (combine-cords overlap-cords1 overlap-cords2
                                           :remove-outliers remove-outliers))
        (dbg :2mat "Done (~D)." (length overlap-cords))
        (dbg :2mat "Building tree...")
        (ultrametric-lasso3 (nconc cords1 cords2 overlap-cords))
          )
        )
    )
  )

(defun random-submatrix (matrix n kept-names)
  (assert (>= n (length kept-names)))
  (let* ((n (- n (length kept-names)))
         (rest (select-random
                (set-difference (names matrix) kept-names :test #'equal)
                n)))
    (sub-matrix matrix (append kept-names rest) :test #'equal)))

