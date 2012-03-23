;;; the BUILD algorithm assembles a supertree from some trees if they are
;;; compatible

(in-package :gk-trees)

;;; returns list of leaf nodes
(defun leafset (tree)
  (cond
    ((listp tree)
     (loop for child in (car tree) append
          (leafset child)))
    (t
     (list tree))))

;;; returns list choose 3
(defun choose3 (list)
  (loop for one on list append
       (loop for two on (cdr one) append
            (loop for three on (cdr two) collecting
                 (list (car one) (car two) (car three))))))

;;; checks if leaf is member of tree
(defun tree-memberp (tree leaf &optional (test #'equal))
  (cond ((cherry-binaryp tree)
         (or
          (tree-memberp (left-child tree) leaf test)
          (tree-memberp (right-child tree) leaf test)))
        ((listp tree)
         (member leaf (car tree) :test test))
        (t
         (funcall test leaf tree))))

;;; returns T if any one of the supplied leaves is in tree
(defun tree-any-membersp (tree leaves &optional (test #'equal))
  (some #'(lambda (leaf) (tree-memberp tree leaf test)) leaves))

;;; this returns the tree with any degree one vertices removed
(defun tree-suppress-degree-twos (tree)
  (cond
    ((= (vertex-degree tree) 2)
     ;; suppress this node
     (tree-suppress-degree-twos (left-child tree)))
    ((cherry-binaryp tree)
     ;; this is a resolved node, so check its children and add their edge
     ;; weights if they will be suppressed
     (let ((le (left-edge-weight tree))
           (re (right-edge-weight tree)))
       (when (= (vertex-degree (left-child tree)) 2)
         ;; left child will be suppressed so add up chain of edge weights
         (setf le
               (+
                (loop with node = (left-child tree)
                   and len = 0
                   while (= (vertex-degree node) 2) do
                   (pretty-print-tree t node)
                   (setf len (+ len (nth-edge-weight 0 node)))
                   (setf node (nth-child 0 node))
                   finally (return len))
                le)))
       (when (= (vertex-degree (right-child tree)) 2)
         ;; right child will be suppressed
         (setf re (+ re (nth-edge-weight 0 (right-child tree)))))
       (make-proper-cherry
        (tree-suppress-degree-twos (left-child tree))
        le
        (tree-suppress-degree-twos (right-child tree))
        re)))
    (t
     tree)))

;;; this removes all leaves except those given, can leave degree 2 vertices
(defun tree-remove-all-except (tree leaves &optional (test #'equal))
  (cond
    ((cherry-binaryp tree)
     ;; this is a resolved node
     (let ((members-in-left (tree-any-membersp 
                             (left-child tree) leaves test))
           (members-in-right (tree-any-membersp
                              (right-child tree) leaves test)))
       (cond
         ((and members-in-left members-in-right)
          ;; we keep this binary node
          (make-proper-cherry
           (tree-restrict-to (left-child tree) leaves test)
           (left-edge-weight tree)
           (tree-restrict-to (right-child tree) leaves test)
           (right-edge-weight tree)))
         (members-in-left
          ;; we keep only the left bit
          (make-cherry
           (cons
            (tree-restrict-to (left-child tree) leaves test)
            (left-edge-weight tree))))
         (members-in-right
          ;; we keep only the right bit
          (make-cherry
           (cons
            (tree-restrict-to (right-child tree) leaves test)
            (right-edge-weight tree))))
         (t
          nil))))
    (t
     tree)))

;;; restricts tree to the leaves given, returning a proper tree
(defun tree-restrict-to (tree leaves &optional (test #'equal))
  (tree-suppress-degree-twos
   (tree-remove-all-except tree leaves test)))

;;; returns list of all triplets displayed by tree
(defun triplets (tree)
  
  )