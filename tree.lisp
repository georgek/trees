(in-package :gk-trees)

(defparameter pretty-tree-horiz-space 1) ; this should be at least 0
(defparameter pretty-tree-height-mult 2) ; this should be at least 1

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

(defparameter tree-default-weight 1)

(defun make-proper-cherry (a ra b rb)
  "Makes a proper cherry with values a,b edge weights ra, rb."
  `((,a ,b) . (,ra ,rb)))

(defun make-cherry (&rest args)
  "Makes a cherry with values and edge weights given. Arguments should be of
  the form (val . weight)"
  (when (< (length args) 1)
    (error "A cherry must have at least one child!"))
  (let ((cherry (cons nil nil)))
    (loop for p in args do
         (setf (car cherry) (cons (car p) (car cherry)))
         (setf (cdr cherry) (cons (cdr p) (cdr cherry))))
    (setf (car cherry) (nreverse (car cherry)))
    (setf (cdr cherry) (nreverse (cdr cherry)))
    cherry))

(defun vertex-degree (vertex)
  (if (listp vertex)
      (1+ (length (car vertex)))
      1))

(defun cherry-binaryp (vertex)
  (= (vertex-degree vertex) 3))

(defun tripletp (tree)
  (and (cherry-binaryp tree)
       (not (and (listp (left-child tree)) (listp (right-child tree))))
       (or (cherry-binaryp (left-child tree))
           (cherry-binaryp (right-child tree)))))

(defun make-ultrametric-triplet (a b c ab ac)
  "Makes an ultrametric triplet ab|c with the edge weight given."
  (when (>= ab ac)
    (error "The distance ab must be less than the distance ac!"))
  (make-proper-cherry
   (make-proper-cherry a (/ ab 2) b (/ ab 2))
   (- (/ ac 2) (/ ab 2))
   c
   (/ ac 2)))

;;; returns the two leaves of the cherry in a triplet
(defun triplet-get-cherry (triplet)
  (when (not (tripletp triplet))
    (error "Not a triplet!"))
  (cond
    ((and (not (listp (left-child triplet)))
          (cherry-binaryp (right-child triplet)))
     (list (left-child (right-child triplet))
           (right-child (right-child triplet))))
    ((and (not (listp (right-child triplet)))
          (cherry-binaryp (left-child triplet)))
     (list (left-child (left-child triplet))
           (right-child (left-child triplet))))
    (t
     (error "Not triplet!"))))

(defun left-child (tree)
  (first (car tree)))

(defun (setf left-child) (left-child tree)
  (setf (first (car tree)) left-child))

(defun right-child (tree)
  (second (car tree)))

(defun (setf right-child) (right-child tree)
  (setf (second (car tree)) right-child))

(defun left-edge-weight (tree)
  (first (cdr tree)))

(defun (setf left-edge-weight) (edge-weight tree)
  (setf (first (cdr tree)) edge-weight))

(defun right-edge-weight (tree)
  (second (cdr tree)))

(defun (setf right-edge-weight) (edge-weight tree)
  (setf (second (cdr tree)) edge-weight))

(defun nth-child (n tree)
  (nth n (car tree)))

(defun nth-edge-weight (n tree)
  (nth n (cdr tree)))

(defun last-child (tree)
  (car (last (car tree))))

(defun last-edge-weight (tree)
  (car (last (cdr tree))))

(defun tree-total-width (tree)
  (cond ((cherry-binaryp tree)
         (+ (tree-total-width (left-child tree))
            (tree-total-width (right-child tree))
            (* pretty-tree-horiz-space 2)
            1))
        ((= (vertex-degree tree) 2)
         (tree-total-width (nth-child 0 tree)))
        ((listp tree)
         (1+ (loop for v in (car tree) summing
                  (1+ (length (format nil "~a" v))))))
        (t
         (length (format nil "~a" tree)))))

;;; extent is the the length of the horizontal line between the node and the
;;; vertical bar (only for binary)
(defun tree-left-extent (tree)
  (cond
    ((cherry-binaryp (left-child tree))
     (+ (tree-total-width (right-child (left-child tree)))
        (* pretty-tree-horiz-space 2)))
    ((= (vertex-degree (left-child tree)) 2)
     (+
      (tree-right-space (left-child tree))
      pretty-tree-horiz-space))
    (t
     (+ (floor (/ (1- (tree-total-width (left-child tree))) 2))
          pretty-tree-horiz-space))))

(defun tree-right-extent (tree)
  (cond
    ((cherry-binaryp (right-child tree))
     (+ (tree-total-width (left-child (right-child tree)))
        (* pretty-tree-horiz-space 2)))
    ((= (vertex-degree (right-child tree)) 2)
     (+
      (tree-left-space (right-child tree))
      pretty-tree-horiz-space))
    (t
     (+ (ceiling (/ (1- (tree-total-width (right-child tree))) 2))
        pretty-tree-horiz-space))))

;;; space is the amount of space between the left/right edge of the bounding
;;; box of the tree to the vertical line
(defun tree-left-space (tree)
  (cond
    ((cherry-binaryp (nth-child 0 tree))
     (+ (tree-total-width (left-child (nth-child 0 tree)))
        pretty-tree-horiz-space))
    ((= (vertex-degree (nth-child 0 tree)) 2)
     (tree-left-space (nth-child 0 tree)))
    (t
     (ceiling (/ (1- (tree-total-width (nth-child 0 tree))) 2)))))

(defun tree-right-space (tree)
  (cond
    ((cherry-binaryp (last-child tree))
     (+ (tree-total-width (right-child (last-child tree)))
        pretty-tree-horiz-space))
    ((= (vertex-degree (last-child tree)) 2)
     (tree-right-space (last-child tree)))
    (t
     (floor (/ (1- (tree-total-width (right-child tree))) 2)))))

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
  (cond ((cherry-binaryp tree)
         (max (+ (left-edge-weight tree)
                 (tree-total-edge-height (left-child tree)))
              (+ (right-edge-weight tree)
                 (tree-total-edge-height (right-child tree)))))
        ((= (vertex-degree tree) 2)
         (+ (nth-edge-weight 0 tree)
            (tree-total-edge-height (nth-child 0 tree))))
        (t
         0)))

(defun tree-height (tree)
  (tree-total-edge-height tree))

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

;;; pretty prints list of trees using up to 80 chars of horizontal space
(defun pretty-print-trees (output trees)
  (let ((tree-list trees))
    (loop while tree-list do
         ;; pop off as many as possible
         (let ((this-row nil)
               (total-width 0)
               (horiz-space (max pretty-tree-horiz-space 1)))
           (loop while tree-list do
                (if (< (+ total-width (tree-total-width (car tree-list)) 1)
                       80)
                    (progn
                      (setf total-width
                            (+ total-width
                               (tree-total-width (car tree-list))
                               horiz-space))
                      (setf this-row (cons (car tree-list) this-row))
                      (setf tree-list (cdr tree-list)))
                    (return)))
           ;; process this row
           (let* (
                  (total-height (loop for tr in this-row maximize
                                     (tree-total-height tr)))
                  (current-pos 0)
                  (matrix (make-array
                           (list total-height
                                 total-width)
                           :initial-element #\Space)))
             (loop for tree in this-row do
                  (put-tree-in-matrix
                   tree (- total-height (tree-total-height tree))
                   current-pos matrix)
                  (setf current-pos (+ current-pos
                                       (tree-total-width tree)
                                       horiz-space)))
             ;; print the matrix out
             (loop for i from 0 to (1- total-height) do
                  (loop for j from 0 to (1- total-width) do
                       (format output "~a" (aref matrix i j)))
                  (format output "~%")))))))

;;; this writes the tree into matrix recursively with its "canvas" beginning
;;; at top left
(defun put-tree-in-matrix (tree top left matrix)
  (cond ((cherry-binaryp tree)
         ;; it's a binary internal node
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
                               matrix)))
        ((= (vertex-degree tree) 2)
         ;; a degree two internal node
         (let* ((node (tree-left-space tree))
                (h (* (nth-edge-weight 0 tree) pretty-tree-height-mult)))
           ;; node
           (setf (aref matrix top (+ left node))
                 pretty-tree-node-char)
           ;; vertical line
           (loop for j from (1+ top) to (+ top h -2) do
                (setf (aref matrix j (+ left node))
                      pretty-tree-vert-char)) ;line
           (when (> h 1)
             (setf (aref matrix (+ top h -1) (+ left node))
                   pretty-tree-vert-end-char)) ;end
           
           ;; print child
           (put-tree-in-matrix (nth-child 0 tree) (+ top h) left matrix)))
        ((consp tree)
         ;; it's a node with n>2 children (unresolved)
         (let ((str (format nil "{~{~a~^,~}}" (car tree))))
           (loop for i from 0 to (1- (length str)) do
                (setf (aref matrix top (+ left i)) (elt str i)))))
        (t
         ;; it's a leaf
         (let ((str (format nil "~a" tree)))
           (loop for i from 0 to (1- (length str)) do
                (setf (aref matrix top (+ left i)) (elt str i)))))))

(defmacro utree (form)
  "Makes an ultrametric tree."
  (cond ((consp form)
         `(make-cherry ,@(mapcar (lambda (f) `(cons (utree ,f)
                                                    ,(if (consp f)
                                                         (- (first form)
                                                            (first f))
                                                         (first form))))
                                 (rest form))))
        ((symbolp form)
         `(quote ,form))
        (t
         form)))

(defun range (beg end)
  "Returns range of numbers between beg and end."
  (assert (<= beg end))
  (loop for i from beg to end collecting i))

(defun make-random-binary (x)
  "Makes a random binary tree with leafset X"
  (let ((n (length x))
        a b h)
    (loop while (> n 1) do
         (setf a (nth (random n) x))
         (setf x (remove a x :test #'equal))
         (setf n (length x))
         (setf b (nth (random n) x))
         (setf x (remove b x :test #'equal))
         (setf n (length x))
         (setf h (1+ (max (tree-height a) (tree-height b))))
         (setf x (append x (list (make-proper-cherry a
                                                     (- h (tree-height a))
                                                     b
                                                     (- h (tree-height b))))))
         (incf n))
    (car x)))
