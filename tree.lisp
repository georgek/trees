(in-package :gk-trees)

(defun make-cherry (a b ra rb)
  "Makes a cherry with edge weights ra and rb."
  `((,a . ,b) . (,ra . ,rb)))

(defun make-ultrametric-triplet (a b c ab ac)
  "Makes an ultrametric triplet ab|c with the edge weight given."
  (make-cherry
   (make-cherry a b (/ ab 2) (/ ab 2))
   c
   (- (/ ac 2) (/ ab 2))
   (/ ac 2)))

(defun left-child (tree)
  (caar tree))

(defun right-child (tree)
  (cdar tree))

(defun left-edge-weight (tree)
  (cadr tree))

(defun right-edge-weight (tree)
  (cddr tree))

(defparameter pretty-tree-horiz-space 2) ; this should be at least 1
(defparameter pretty-tree-height-mult 2) ; this should probably be at least 2

;;; unicode set
(defparameter pretty-tree-horiz-char #\─)
(defparameter pretty-tree-vert-char #\│)
(defparameter pretty-tree-vert-end-char #\│)
(defparameter pretty-tree-left-corner-char #\╭)
(defparameter pretty-tree-right-corner-char #\╮)
(defparameter pretty-tree-node-char #\●)

;;; ascii set
;; (defparameter pretty-tree-horiz-char #\-)
;; (defparameter pretty-tree-vert-char #\|)
;; (defparameter pretty-tree-vert-end-char #\|)
;; (defparameter pretty-tree-left-corner-char #\+)
;; (defparameter pretty-tree-right-corner-char #\+)
;; (defparameter pretty-tree-node-char #\^)

(defun tree-total-width (tree)
  (if (listp tree)
      (+ (tree-total-width (left-child tree))
         (tree-total-width (right-child tree))
         (* pretty-tree-horiz-space 2)
         1)
      (length (format nil "~a" tree))))

;;; extent is the the length of the horizontal line between the node and the
;;; vertical bar
(defun tree-left-extent (tree)
  (if (listp (left-child tree))
      (+ (tree-total-width (right-child (left-child tree)))
         (* pretty-tree-horiz-space 2))
      (+ (floor (/ (1- (tree-total-width (left-child tree))) 2))
         pretty-tree-horiz-space)))

(defun tree-right-extent (tree)
  (if (listp (right-child tree))
      (+ (tree-total-width (left-child (right-child tree)))
         (* pretty-tree-horiz-space 2))
      (+ (ceiling (/ (1- (tree-total-width (right-child tree))) 2))
         pretty-tree-horiz-space)))

;;; space is the amount of space between the left/right edge of the bounding box of the tree to the vertical line
(defun tree-left-space (tree)
  (if (listp (left-child tree))
      (+ (tree-total-width (left-child (left-child tree)))
         pretty-tree-horiz-space)
      (ceiling (/ (1- (tree-total-width (left-child tree))) 2))))

(defun tree-right-space (tree)
  (if (listp (right-child tree))
      (+ (tree-total-width (right-child (right-child tree)))
         pretty-tree-horiz-space)
      (floor (/ (1- (tree-total-width (right-child tree))) 2))))

;;; this just makes sure the widths agree for a tree
(defun tree-width-test (tree)
  (= (tree-total-width tree)
     (+ (tree-left-space tree)
        1
        (tree-left-extent tree)
        1
        (tree-right-extent tree)
        1
        (tree-right-space tree))))

;;; total height of tree is longest path from root to a leaf in terms of edge
;;; weights
(defun tree-total-edge-height (tree)
  (if (listp tree)
      (max (+ (left-edge-weight tree)
              (tree-total-edge-height (left-child tree)))
           (+ (right-edge-weight tree)
              (tree-total-edge-height (right-child tree))))
      0))

;;; total height of pretty printed tree
(defun tree-total-height (tree)
  (1+ (* (tree-total-edge-height tree) pretty-tree-height-mult)))

(defun pretty-print-tree (output tree)
  "Pretty prints a tree."
  (let ((matrix (make-array
                 (list (tree-total-height tree)
                       (tree-total-width tree))
                 :initial-element #\Space)))
    
    ;; fill the matrix
    (put-tree-in-matrix tree 0 0 matrix)
    
    ;; print the matrix out
    (loop for i from 0 to (1- (tree-total-height tree)) do
         (loop for j from 0 to (1- (tree-total-width tree)) do
              (format output "~a" (aref matrix i j)))
         (format output "~%"))))

;;; this writes the tree into matrix recursively with its "canvas" beginning
;;; at top left
(defun put-tree-in-matrix (tree top left matrix)
  (if (listp tree) 
      ;; it's an internal node
      (let* ((ls (tree-left-space tree))
             (le (tree-left-extent tree))
             (re (tree-right-extent tree))
             (lh (* (left-edge-weight tree) pretty-tree-height-mult))
             (rh (* (right-edge-weight tree) pretty-tree-height-mult))
             (node (+ ls le 1)))
        ;; horizontal line
        (setf (aref matrix top (+ left ls))
              pretty-tree-left-corner-char) ;left end of left line
        (loop for i from (+ left ls 1) to (+ left ls le) do
             (setf (aref matrix top i)
                   pretty-tree-horiz-char))      ;left line
        (setf (aref matrix top (+ left node))
              pretty-tree-node-char) ;node
        (loop for i from (+ left node 1) to (+ left node re) do
             (setf (aref matrix top i)
                   pretty-tree-horiz-char))           ;right line
        (setf (aref matrix top
                    (+ left node re 1))
              pretty-tree-right-corner-char) ;right end of right line
        
        ;; vertical lines
        (loop for j from (1+ top) to (+ top lh -2) do
             (setf (aref matrix j (+ left ls))
                   pretty-tree-vert-char)) ;left line
        (when (> lh 1)
          (setf (aref matrix (+ top lh -1) (+ left ls))
                pretty-tree-vert-end-char)) ;left end
        (loop for j from (1+ top) to (+ top rh -2) do
             (setf (aref matrix j (+ left ls le 2 re))
                   pretty-tree-vert-char)) ;right line
        (when (> rh 1)
          (setf (aref matrix (+ top rh -1) (+ left ls le 2 re))
                pretty-tree-vert-end-char)) ;right end
        
        ;; print children
        (put-tree-in-matrix (left-child tree) (+ top lh) left matrix)
        (put-tree-in-matrix (right-child tree) (+ top rh)
                            (+ left node 1 pretty-tree-horiz-space)
                            matrix))
      
      ;; it's a leaf
      (let ((str (format nil "~a" tree)))
        (loop for i from 0 to (1- (length str)) do
             (setf (aref matrix top (+ left i)) (elt str i))))))

