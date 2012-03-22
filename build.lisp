;;; the BUILD algorithm assembles a supertree from some trees if they are
;;; compatible

(in-package :gk-trees)

;;; returns list of leaf nodes
(defun leafset (tree)
  (if (listp tree)
      (append (leafset (left-child tree)) (leafset (right-child tree)))
      (list tree)))

;;; returns list choose 3
(defun choose3 (list)
  (loop for one on list append
       (loop for two on (cdr one) append
            (loop for three on (cdr two) collecting
                 (list (car one) (car two) (car three))))))

;;; checks if leaf is member of tree
(defun memberp (tree leaf &optional (comparison #'equal))
  (if (listp tree)
      (or (memberp (left-child tree) leaf) (memberp (right-child tree) leaf))
      (funcall comparison leaf tree)))

;;; returns list of all triplets displayed by tree
(defun triplets (tree)
  
  )