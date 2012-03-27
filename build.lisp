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
(defun tree-member (tree leaf &optional (test #'equal))
  (cond ((cherry-binaryp tree)
         (or
          (tree-member (left-child tree) leaf test)
          (tree-member (right-child tree) leaf test)))
        ((listp tree)
         (member leaf (car tree) :test test))
        (t
         (funcall test leaf tree))))

;;; returns T if any one of the supplied leaves is in tree
(defun tree-any-members (tree leaves &optional (test #'equal))
  (some #'(lambda (leaf) (tree-member tree leaf test)) leaves))

(defun tree-all-members (tree leaves &optional (test #'equal))
  (every #'(lambda (leaf) (tree-member tree leaf test)) leaves))

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
     (let ((members-in-left (tree-any-members 
                             (left-child tree) leaves test))
           (members-in-right (tree-any-members
                              (right-child tree) leaves test)))
       (cond
         ((and members-in-left members-in-right)
          ;; we keep this binary node
          (make-proper-cherry
           (tree-remove-all-except (left-child tree) leaves test)
           (left-edge-weight tree)
           (tree-remove-all-except (right-child tree) leaves test)
           (right-edge-weight tree)))
         (members-in-left
          ;; we keep only the left bit
          (make-cherry
           (cons
            (tree-remove-all-except (left-child tree) leaves test)
            (left-edge-weight tree))))
         (members-in-right
          ;; we keep only the right bit
          (make-cherry
           (cons
            (tree-remove-all-except (right-child tree) leaves test)
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
(defun tree-all-triplets (tree)
  (let ((triples (choose3 (leafset tree))))
    (loop for triple in triples collecting
         (tree-restrict-to tree triple))))

(defun restrict-triplets-to (triplets leaves &optional (test #'equal))
  (remove-if-not #'tripletp
                 (loop for triplet in triplets collecting
                      (tree-restrict-to triplet leaves test))))

;;; uses BUILD algorithm to build a supertree from the given trees if they are
;;; compatible and if they are not it returns NIL
(defun build-from-triplets (triplets)
  (let* ((tree
          (loop with l = () for triplet in triplets do
               (setf l (union l (leafset triplet)))
             finally (return l)))
         (graph
          (loop for l in tree collecting (list l))))
    
    ;; connect graph
    (loop with cherry and to-merge
       for triplet in triplets do
         (setf
          cherry
          (triplet-get-cherry triplet))
         (setf
          to-merge
          (remove-if-not
           #'(lambda (l) (or (member (first cherry) l)
                             (member (second cherry) l)))
           graph))
         (when (> (length to-merge) 1)
           (loop for merge in to-merge do
                (setf graph (delete merge graph :test #'equalp)))
           (setf graph (cons (loop for m in to-merge append m) graph))))
    
    (when (< (length graph) 2)
      (error "Trees are not compatible!"))
    
    (setf tree graph)
    (loop with restricted-triplets
       for child on tree do
         (cond
           ((> (length (car child)) 2)  ; try to resolve this further
            (setf
             restricted-triplets
             (restrict-triplets-to triplets (car child)))
            (setf (car child) (build-from-triplets restricted-triplets)))
           ((= (length (car child)) 2)  ; proper cherry
            (setf
             (car child)
             (cons (car child)
                   (make-list (length (car child))
                              :initial-element tree-default-weight))))
           (t                           ; single leaf
            (setf (car child) (caar child)))))

    (cons
     tree
     (make-list (length tree) :initial-element tree-default-weight))))

(defun build (trees)
  (build-from-triplets
   (loop for tree in trees append
        (tree-all-triplets tree))))

