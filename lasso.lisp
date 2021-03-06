(in-package :gk-trees)

(defclass cord ()
  ((left
    :initarg :left
    :initform (error "Must provide a left side.")
    :accessor cord-left)
   (right
    :initarg :right
    :initform (error "Must provide a right side.")
    :accessor cord-right)
   (length
    :initarg :length
    :initform 1
    :accessor cord-length)))

(defmethod print-object ((object cord) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A : ~A = ~A" (cord-left object) (cord-right object)
            (cord-length object))))

(defun cord (a b d)
  "Makes a cord ab=d."
  (make-instance 'cord :left a :right b :length d))

(defmacro ucord (a b)
  `(cord ',a ',b 1))

(defmacro cords (&rest cords)
  `',(mapcar (lambda (c) (cord (car c) (cadr c) (caddr c))) cords))

(defun cords-equal (cord1 cord2 &optional (test #'eq))
  (or (and (funcall test (cord-left cord1) (cord-left cord2))
           (funcall test (cord-right cord1) (cord-right cord2)))
      (and (funcall test (cord-left cord1) (cord-right cord2))
           (funcall test (cord-right cord1) (cord-left cord2)))))

(defun cords-vertices (cords)
  (remove-duplicates (nconc (mapcar #'cord-left cords)
                            (mapcar #'cord-right cords))
                     :test #'eq))

(defun cord-contains-p (cord vertex)
  (or (eq (cord-left cord) vertex)
      (eq (cord-right cord) vertex)))

(defun cord-intersection (cord1 cord2 &optional (test #'eq))
  (intersection
   (list (caar cord1) (cdar cord1))
   (list (caar cord2) (cdar cord2))
   :test test))

(defun cord-exclusive-or (cord1 cord2 &optional (test #'eq))
  (set-exclusive-or
   (list (caar cord1) (cdar cord1))
   (list (caar cord2) (cdar cord2))
   :test test))

(defun cord-difference (cord1 cord2 &optional (test #'eq))
  (set-difference
   (list (caar cord1) (cdar cord1))
   (list (caar cord2) (cdar cord2))
   :test test))

;;; this returns all cords which span this tree
(defun restrict-cords-to-tree (tree cords)
  (union
   (intersection
    (remove-if-not
     #'(lambda
           (cord)
         (tree-all-members (left-child tree) (list (cord-left cord))))
     cords)
    (remove-if-not
     #'(lambda
           (cord)
         (tree-all-members (right-child tree) (list (cord-right cord))))
     cords))
   (intersection
    (remove-if-not
     #'(lambda(cord)
         (tree-all-members (right-child tree) (list (cord-left cord))))
     cords)
    (remove-if-not
     #'(lambda(cord)
         (tree-all-members (left-child tree) (list (cord-right cord))))
     cords))))

(defun ultrametric-distance-to-bottom (tree)
  (loop with distance = 0
     while (> (vertex-degree tree) 1) do
       (setf distance (+ distance (left-edge-weight tree)))
       (setf tree (left-child tree))
     finally (return distance)))

(defun ultrametric-tree-set-weights (tree cords)
  (let ((deg (vertex-degree tree)))
    (cond
      ((= deg 3)
       (let ((new-left-child
              (ultrametric-tree-set-weights (left-child tree) cords))
             (new-right-child
              (ultrametric-tree-set-weights (right-child tree) cords))
             (distance-to-bottom
              (/
               (cord-length (first (restrict-cords-to-tree tree cords)))
               2)))
         (make-proper-cherry
          new-left-child
          (- distance-to-bottom
             (ultrametric-distance-to-bottom new-left-child))
          new-right-child
          (- distance-to-bottom
             (ultrametric-distance-to-bottom new-right-child)))))
      (t
       tree))))

(defun ultrametric-lasso (&rest cords)
  "Lassos an ultrametric tree from the given cords.  A cord, ab=d should be of
  the form ((a . b) . d)."
  (let ((triplets nil)
        (tree nil))
    ;; assemble triplets
    (loop with shorter-cords
       for cord in cords do
         (setf
          shorter-cords
          (remove-if
           #'(lambda (c) (>= (cord-length c) (cord-length cord)))
           cords))
         (loop for shorter-cord in shorter-cords
            for i = (cord-intersection shorter-cord cord)
            and ld = (cord-difference shorter-cord cord)
            and rd = (cord-difference cord shorter-cord) do
              (cond
                ((= (length i) 1)
                 (setf triplets
                       (cons
                        (make-ultrametric-triplet
                         (car ld) (car i) (car rd)
                         (cord-length shorter-cord)
                         (cord-length cord))
                        triplets)))
                (t
                 nil))))
    (pretty-print-trees t triplets)
    
    (setf tree (build-from-triplets triplets))
    
    (ultrametric-tree-set-weights tree cords)))

(defun b-remove-if (test list)
  (let ((removed (remove-if test list)))
    (values removed (set-difference list removed :test #'eq))))

(defun b-remove-if2 (test vector)
  (let ((not-removed (remove-if test vector))
        (removed (remove-if-not test vector)))
    (values not-removed removed)))

(defun cord-other-end (cord this-end)
  (assert (cord-contains-p cord this-end))
  (if (eq (cord-left cord) this-end)
      (cord-right cord)
      (cord-left cord)))

(defun component (cords vertex)
  (let ((vertices (list vertex))
        (component (list))
        (rest (list)))
    (loop while (consp vertices)
       for current-vertex = (pop vertices) do
         (loop for cord in cords do
              (if (cord-contains-p cord current-vertex)
                  (progn
                    (push cord component)
                    (push (cord-other-end cord current-vertex) vertices))
                  (push cord rest)))
         (setf cords rest
               rest (list)))
    (values component cords)))

(defun components (cords)
  "Returns a list of lists of cords which are connected."
  (let ((components (list)))
    (loop while (consp cords) do
         (multiple-value-bind (component rest)
             (component cords (cord-left (first cords)))
           (push component components)
           (setf cords rest)))
    components))

(defun complete-p (cords)
  "Returns true if graph is complete."
  (let ((n (length (cords-vertices cords)))
        (m (length (remove-duplicates cords :test #'cords-equal))))
    (= m (/ (* n (- n 1)) 2))))

(defclass lassoed-tree (tree)
  ((used-cords
    :initarg :used-cords
    :initform (list)
    :accessor used-cords
    :documentation "The cords that were used to lasso this tree.")))

(defgeneric lassoed-tree-used-cords (tree)
  (:documentation "All cords used to lasso this tree."))

(defmethod lassoed-tree-used-cords ((tree lassoed-tree))
  (append (used-cords tree) (reduce #'append
                                    (mapcar #'lassoed-tree-used-cords
                                            (children tree)))))

(defmethod lassoed-tree-used-cords (tree)
  (list))

(defun collapse (cords)
  (let ((height (cord-length (first cords)))
        (children (cords-vertices cords)))
    (assert (every (lambda (c) (= height (cord-length c))) cords))
    (make-instance
     'lassoed-tree
     :children children
     :edge-weights (mapcar
                    (lambda (c) (- (/ height 2)
                                   (tree-height c)))
                    children)
     :used-cords (reduce #'nconc (mapcar #'real-cords cords)))))

(defun ultrametric-lasso2 (cords)
  (let (tree)
    (loop while (consp cords)
       for min = (reduce #'min cords :key #'cord-length)
       do
         (multiple-value-bind (mins rest)
             (b-remove-if (lambda (c) (/= (cord-length c) min)) cords)
           (setf cords rest)
           (loop for component in (components mins) do
                (unless (complete-p component)
                  (dbg :lasso2 "Not a clique: ~A~%" component)
                  (return-from ultrametric-lasso2 nil))
                (setf tree (collapse component))
                (when (dbg-on-p :lasso2)
                  (pp-tree-print tree :vertical t))
                (let ((other-leaves (make-hash-table :test #'eq))
                      other-leaf)
                  (loop for cord in cords do
                       (cond ((consp (intersection (leafset (cord-left cord))
                                                   (leafset tree)))
                              (setf other-leaf #'cord-right)
                              (setf (cord-left cord) tree))
                             ((consp (intersection (leafset (cord-right cord))
                                                   (leafset tree)))
                              (setf other-leaf #'cord-left)
                              (setf (cord-right cord) tree))
                             (t
                              (setf other-leaf nil)))
                       (when (functionp other-leaf)
                         (if (gethash (funcall other-leaf cord) other-leaves)
                             (unless (= (gethash (funcall other-leaf cord)
                                                 other-leaves)
                                        (cord-length cord))
                               (dbg :lasso2 "Not ultrametric.~%")
                               (return-from ultrametric-lasso2 nil))
                             (setf (gethash (funcall other-leaf cord)
                                            other-leaves)
                                   (cord-length cord)))))
                  (setf cords (remove-duplicates cords :test #'cords-equal))))))
    tree))

(defun shuffle-bad (list)
  "Incorrect shuffle, but good enough."
  (sort list #'> :key (lambda (x) (declare (ignore x)) (random 1.0))))

(defun shuffle (list)
  "Shuffles a list using Fisher-Yates shuffle."
  (let* ((n (length list))
         (items (make-array n)))
    (setf (aref items 0) (car list))
    (loop for i from 1 below n
       for listi in (cdr list)
       for j = (random (1+ i))
       do
         (when (/= j i)
           (setf (aref items i) (aref items j)))
         (setf (aref items j) listi))
    (map 'list #'identity items)))

(defparameter maxi-clique-iterations 10
  "Number of times to find a random maximal clique.")

(defun clique-leaves (cords)
  "Number of leaves that will be obtained from using this clique."
  (reduce #'+ (mapcar #'leafset (cords-vertices cords)) :key #'length))

(defun maxi-clique (cords)
  "Trys to find a large clique in terms of overall number of leaves."
  (loop with max-leaves = 0 with max-clique
     repeat maxi-clique-iterations do
       (let* ((vertices (shuffle (cords-vertices cords)))
              (cords (copy-list cords))
              (clique-vertices (list (pop vertices)))
              (clique-cords (list)))
         (loop while (consp vertices)
            for potential = (pop vertices) do
              (multiple-value-bind (rest join-cords)
                  (b-remove-if (lambda (c) (or (and (eq (cord-left c) potential)
                                                    (member (cord-right c)
                                                            clique-vertices
                                                            :test #'eq))
                                               (and (eq (cord-right c) potential)
                                                    (member (cord-left c)
                                                            clique-vertices
                                                            :test #'eq))))
                               cords)
                (when (= (length join-cords) (length clique-vertices))
                  (push potential clique-vertices)
                  (setf clique-cords (nconc clique-cords join-cords)))
                (setf cords rest)))
         (dbg :maxi-clique "Clique: ~A~%" (clique-leaves clique-cords))
         (when (> (clique-leaves clique-cords) max-leaves)
           (setf max-clique clique-cords
                 max-leaves (clique-leaves max-clique))))
       finally (return max-clique)))

(defclass collapsed-cord (cord)
  ((real-cords
    :initarg :real-cords
    :initform (list)
    :accessor real-cords
    :documentation "The real cords that were collapsed to this cord.")))

(defmethod initialize-instance :after ((cord collapsed-cord) &key)
  (when (null (real-cords cord))
    (setf (real-cords cord) (list (cord (cord-left cord)
                                        (cord-right cord)
                                        (cord-length cord))))))

(defgeneric collapsed-cord (object))

(defmethod collapsed-cord ((cord cord))
  (make-instance 'collapsed-cord :left (cord-left cord) :right (cord-right cord)
                 :length (cord-length cord) :real-cords nil))

(defmethod real-cords ((cord cord))
  (list cord))

(defun mean (list)
  (dbg :mean "mean diff: ~D~%" (- (reduce #'max list) (reduce #'min list)))
  (/ (reduce #'+ list) (length list)))

(defun max-from-mean (list)
  (let ((mean (mean list)))
   (reduce #'max (mapcar (lambda (n) (abs (- n mean))) list))))

(defun cluster-goodness (list)
  (/ (max-from-mean list) (mean list)))

(defun median (list)
  (let* ((list (sort (copy-list list) #'<))
         (n (length list))
         (mid (nthcdr (floor (/ (1- n) 2)) list)))
    (if (/= 0 (rem n 2))
        (car mid)
        (/ (+ (car mid) (cadr mid)) 2))))

(defun low-quartile (list)
  (let ((med (median list)))
   (median (remove-if (lambda (x) (> x med)) list))))

(defun high-quartile (list)
  (let ((med (median list)))
   (median (remove-if (lambda (x) (< x med)) list))))

(defun remove-outliers (list &optional (key #'identity))
  "Returns the list with outliers removed or, if all elements are outliers,
return just the mode."
  (let* ((lq (low-quartile (mapcar key list)))
         (hq (high-quartile (mapcar key list)))
         (iqr (- hq lq)))
    (remove-if (lambda (x) (or (> (funcall key x) (+ hq (* 0.5 iqr)))
                               (< (funcall key x) (- lq (* 0.5 iqr)))))
               list)))

;;; cluster should have goodness below this
(defparameter cluster-goodness-threshold 0.1)

(defun get-cluster (list &optional (key #'identity))
  "Returns one cluster which has goodness below threshold."
  (flet ((dist (x y) (abs (- x y))))
   (loop for keys = (mapcar key list)
      for mean = (mean keys)
      for max = (reduce #'max (mapcar (lambda (k) (dist k mean)) keys))
      for furthest = (find max keys :key (lambda (k) (dist k mean)) :test #'=)
      for goodness = (/ max mean)
      while (> goodness cluster-goodness-threshold)
      do
        (dbg :clus "~A~%" furthest)
        (setf list (remove furthest list :key key))))
  list)

(defun mode (list)
  (let ((counts (make-hash-table)))
    (dbg :mode "~{~A~%~}~%" list)
    (loop for item in list do
         (if (gethash item counts)
             (incf (gethash item counts))
             (setf (gethash item counts) 1)))
    (loop with max-length = 0 with max
       for item being the hash-keys in counts do
         (when (> (gethash item counts) max-length)
           (setf max item
                 max-length (gethash item counts)))
       finally (return max))))

(defvar *dbg-collapse-cords* nil)

(defun collapse-cords (cords collapsed-tree)
  (let ((clique-vertices (if (consp (label collapsed-tree))
                             (label collapsed-tree)
                             (children collapsed-tree)))
        (collapsed-cords (make-hash-table :test #'eq))
        (final-cords (list))
        (rest (list)))
    ;; (dbg :coll-cords "Component: ~A~%Clique: ~A~%" component-vertices
    ;;      clique-vertices)
    (loop for cord in cords do
         (cond
           ((find (cord-left cord) clique-vertices)
            (push cord (gethash (cord-right cord) collapsed-cords)))
           ((find (cord-right cord) clique-vertices)
            (push cord (gethash (cord-left cord) collapsed-cords)))
           (t
            (push cord rest))))
    (loop for other-end being the hash-keys in collapsed-cords
       for real-cords = (remove-outliers (gethash other-end collapsed-cords)
                                         #'cord-length)
       ;; for real-cords = (gethash other-end collapsed-cords)
       ;; for real-cords = (get-cluster (gethash other-end collapsed-cords)
       ;;                               #'cord-length)
       for length = (mean (mapcar #'cord-length real-cords))
       do
         ;; (dbg :coll-cords "Diff: ~,4F~%" (- (reduce #'max (mapcar #'cord-length real-cords))
         ;;                                    (reduce #'min (mapcar #'cord-length real-cords))))
         ;; (dbg :coll-cords "Length: ~,4F~%" length)
         (if (dbg-on-p :coll-cords)
             (push (mapcar #'cord-length real-cords) *dbg-collapse-cords*))
         (push (make-instance 'collapsed-cord
                              :left collapsed-tree :right other-end
                              :length length
                              :real-cords (reduce #'append
                                                  (mapcar #'real-cords
                                                          real-cords)))
               final-cords))
    (nconc final-cords rest)))

(defun collapse-cords2 (cords collapsed-tree)
  (let ((clique-vertices (if (consp (label collapsed-tree))
                             (label collapsed-tree)
                             (children collapsed-tree)))
        (collapsed-cords (make-hash-table :test #'eq))
        (final-cords (list))
        (rest (list)))
    ;; (dbg :coll-cords "Component: ~A~%Clique: ~A~%" component-vertices
    ;;      clique-vertices)
    (loop for cord across cords do
         (cond
           ((find (cord-left cord) clique-vertices)
            (push cord (gethash (cord-right cord) collapsed-cords)))
           ((find (cord-right cord) clique-vertices)
            (push cord (gethash (cord-left cord) collapsed-cords)))
           (t
            (push cord rest))))
    (loop for other-end being the hash-keys in collapsed-cords
       ;; for real-cords = (remove-outliers (gethash other-end collapsed-cords)
       ;;                                   #'cord-length)
       ;; for real-cords = (gethash other-end collapsed-cords)
       for real-cords = (get-cluster (gethash other-end collapsed-cords)
                                     #'cord-length)
       for length = (mean (mapcar #'cord-length real-cords))
       do
         ;; (dbg :coll-cords "Diff: ~,4F~%" (- (reduce #'max (mapcar #'cord-length real-cords))
         ;;                                    (reduce #'min (mapcar #'cord-length real-cords))))
         ;; (dbg :coll-cords "Length: ~,4F~%" length)
         (if (dbg-on-p :coll-cords)
             (push (mapcar #'cord-length real-cords) *dbg-collapse-cords*))
         (push (make-instance 'collapsed-cord
                              :left collapsed-tree :right other-end
                              :length length
                              :real-cords (reduce #'append
                                                  (mapcar #'real-cords
                                                          real-cords)))
               final-cords))
    (nconc final-cords rest)))

(defun ultrametric-lasso3 (cords)
  (assert (= 1 (length (components cords))))
  (let (tree
        (cords (mapcar #'collapsed-cord cords)))
    (loop while (consp cords)
       for min = (reduce #'min cords :key #'cord-length)
       do
         (dbg :lasso3 "Min: ~A~%" min)
         (dbg :lasso3 "Cords: ~A~%" cords)
         (multiple-value-bind (mins rest)
             (b-remove-if (lambda (c) (/= (cord-length c) min)) cords)
           (setf cords rest)
           (loop for component in (components mins) do
                (setf tree (collapse (maxi-clique component)))
                (dbg :lasso3 "Tree used cords: ~A~%" (used-cords tree))
                (when (dbg-on-p :lasso3)
                  (pp-tree-print tree :vertical t))
                (setf cords (collapse-cords cords tree)))
           ;; choose biggest component (can save others here)
           (setf cords (reduce (lambda (c1 c2) (if (> (clique-leaves c1)
                                                      (clique-leaves c2))
                                                   c1
                                                   c2))
                               (components cords) :initial-value nil))))
    tree))

(defun ultrametric-lasso4 (cords)
  (let (tree
        (forest (make-instance 'queue :comparison #'> :key #'nleaves))
        (cords (make-instance
                'queue
                :initial-contents (map 'vector #'collapsed-cord cords)
                :comparison #'<
                :key #'cord-length)))
    (loop until (queue-empty-p cords)
       for mins = (dequeue-top-to-list cords)
       do
         (dbg :lasso4 "Cords: ~A~%" cords)
         (loop for component in (components mins) do
              (setf tree (collapse (maxi-clique component)))
              (enqueue tree forest)
              (dbg :lasso4 "Tree used cords: ~A~%" (used-cords tree))
              (when (dbg-on-p :lasso4)
                (pp-tree-print tree :vertical t))
              (setf cords (make-instance
                           'queue
                           :initial-contents
                           (collapse-cords2 (slot-value cords 'array) tree)
                           :comparison #'<
                           :key #'cord-length))))
    forest))

