(in-package :gk-trees)

(defun cord (a b d)
  "Makes a cord ab=d."
  (cons (cons a b) d))

(defmacro ucord (a b)
  `(cord ',a ',b 1))

(defun cord-left (cord)
  (caar cord))

(defun (setf cord-left) (cord-left cord)
  (setf (caar cord) cord-left))

(defun cord-right (cord)
  (cdar cord))

(defun (setf cord-right) (cord-right cord)
  (setf (cdar cord) cord-right))

(defun cord-length (cord)
  (cdr cord))

(defun (setf cord-length) (cord-length cord)
  (setf (cdr cord) cord-length))

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

(defun cord-intersection (cord1 cord2 &optional (test #'equal))
  (intersection
   (list (caar cord1) (cdar cord1))
   (list (caar cord2) (cdar cord2))
   :test test))

(defun cord-exclusive-or (cord1 cord2 &optional (test #'equal))
  (set-exclusive-or
   (list (caar cord1) (cdar cord1))
   (list (caar cord2) (cdar cord2))
   :test test))

(defun cord-difference (cord1 cord2 &optional (test #'equal))
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
    (values removed (set-difference list removed))))

(defun component (cords vertex)
  (dbg :component "cords: ~A~%v: ~A~%" cords vertex)
  (multiple-value-bind (rest component)
      (b-remove-if (lambda (c) (cord-contains-p c vertex)) cords)
    (dbg :component "comp: ~A~%rest: ~A~%" component rest)
    (loop for cord in component do
         (setf component (union component (component rest
                                                     (if (eq vertex
                                                             (cord-left cord))
                                                         (cord-right cord)
                                                         (cord-left cord))))))
    (values component (set-difference cords component))))

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

(defun collapse (cords)
  (let ((height (cord-length (first cords)))
        (vertices (list)))
    (assert (every (lambda (c) (= height (cord-length c))) cords))
    (loop for cord in cords do
         (setf vertices (union vertices (list (cord-left cord)
                                              (cord-right cord)))))
    (cons vertices
          (mapcar (lambda (v) (- (/ height 2) (tree-height v))) vertices))))

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
                (pretty-print-tree t tree)
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
                            (setf (gethash (funcall other-leaf cord) other-leaves)
                                  (cord-length cord)))))
                  (setf cords (remove-duplicates cords :test #'cords-equal))))))
    tree))
