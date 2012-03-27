(in-package :gk-trees)

(defun cord (a b d)
  "Makes a cord ab=d."
  (cons (cons a b) d))

(defun cord-left (cord)
  (caar cord))

(defun cord-right (cord)
  (cdar cord))

(defun cord-length (cord)
  (cdr cord))

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

(defun ultrametric-lasso (cords)
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

