(in-package :gk-trees)

(defparameter pretty-tree-horiz-space 2) ; this should be at least 0
(defparameter pretty-tree-height-mult 2) ; this should be at least 1

;;; unicode set
(defparameter pretty-tree-horiz-char #\─)
(defparameter pretty-tree-vert-char #\│)
(defparameter pretty-tree-vert-end-char #\│)
(defparameter pretty-tree-left-corner-char #\╭)
(defparameter pretty-tree-right-corner-char #\╮)
(defparameter pretty-tree-down-char #\┬)
(defparameter pretty-tree-node-char #\●)

;;; ascii set
;; (defparameter pretty-tree-horiz-char #\-)
;; (defparameter pretty-tree-vert-char #\|)
;; (defparameter pretty-tree-vert-end-char #\|)
;; (defparameter pretty-tree-left-corner-char #\+)
;; (defparameter pretty-tree-right-corner-char #\+)
;; (defparameter pretty-tree-node-char #\^)

(defparameter tree-default-weight 1)

(defclass tree ()
  ((label
    :initarg :label
    :initform nil
    :accessor label
    :documentation "The label of this vertex.")
   (children
    :initarg :children
    :initform (list)
    :accessor children
    :documentation "The children of this vertex.")
   (edge-weights
    :initarg :edge-weights
    :initform (list)
    :accessor edge-weights
    :documentation "The edge-weights of the outgoing edges.")))

(defmethod initialize-instance :after ((tree tree) &key)
  ;; ensure that there are enough edge weights
  (cond
    ((numberp (edge-weights tree))
     (setf (edge-weights tree) (make-list (length (children tree))
                                          :initial-element (edge-weights tree))))
    ((consp (edge-weights tree))
     (when (< (length (edge-weights tree)) (length (children tree)))
       (setf (edge-weights tree)
             (nconc (edge-weights tree)
                    (make-list (- (length (children tree))
                                  (length (edge-weights tree)))
                               :initial-element
                               tree-default-weight)))))
    (t
     (setf (edge-weights tree) (make-list (length (children tree))
                                          :initial-element
                                          tree-default-weight)))))

(defun make-tree (tree)
  "Makes a tree from a Lisp tree."
  (if (consp tree)
      (make-instance 'tree :children (mapcar #'make-tree tree))
      (make-instance 'tree :label tree)))

(defmethod print-object ((object tree) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "with ~A leaves" (length (leafset object)))))

(defun pp-tree-print (tree &optional (stream t))
  (let* ((printer (pp-tree-printer tree))
         (next-line (funcall printer nil)))
    (loop while (string/= next-line "") do
         (format stream "~A~%" next-line)
         (setf next-line (funcall printer nil)))))

(defgeneric pp-tree-width (tree))

(defmethod pp-tree-width ((tree tree))
  (max 1
       (length (format nil "~A" (label tree)))
       (+ (reduce #'+ (mapcar #'pp-tree-width
                              (children tree)))
          (* pretty-tree-horiz-space
             (1- (length (children tree)))))))

(defmethod pp-tree-width (tree)
  (length (format nil "~a" tree)))

(defgeneric pp-tree-height (tree))

(defmethod pp-tree-height ((tree tree))
  (1+ (reduce #'max (mapcar #'+ (mapcar #'pp-tree-height (children tree))
                            (edge-weights tree))
              :initial-value 0)))

(defmethod pp-tree-height (tree) 1)

(defun pp-tree-label-line (label width children-widths)
  "Makes a top line string for the label line of a tree.  WIDTH is the width
of this tree, CHILDREN-WIDTHS is a list of widths of each child."
  (if (>= (length label) width)
      label
      (let ((output (make-array width :element-type 'character
                                :fill-pointer 0 :initial-element #\Space))
            (b #\Space)                      ; before
            (d pretty-tree-left-corner-char) ; during
            (a pretty-tree-horiz-char))      ; after
        (loop
           for cwidth on children-widths
           for halfway = (/ (1- (car cwidth)) 2)
           do
             (loop repeat (ceiling halfway) do (format output "~C" b))
             (format output "~C" d)
             (when a
               (loop repeat (+ (floor halfway) pretty-tree-horiz-space)
                  do (format output "~C" a)))
             (if (and (cdr cwidth)
                      (cddr cwidth))
                 (setf b pretty-tree-horiz-char
                       d pretty-tree-down-char)
                 (setf b pretty-tree-horiz-char
                       d pretty-tree-right-corner-char
                       a nil)))
        ;; now put the label in
        (let ((seq-start (ceiling (/ (- width (length label)) 2))))
          (setf (subseq output seq-start (+ seq-start (length label)))
                label))
        (setf (fill-pointer output) width)
        output)))

(defgeneric pp-tree-printer (tree)
  (:documentation "Returns a closure which prints each successive line of
  output for the tree to the given output, or returns it as a string if the
  output is nil.  Prints empty strings when the output is complete."))

(defmethod pp-tree-printer ((tree tree))
  (assert (>= (length (edge-weights tree)) (length (children tree))))
  (let* ((tree-label (if (label tree) (format nil "~A" (label tree))
                         (string pretty-tree-node-char)))
         (tree-width (pp-tree-width tree))
         (children-printers (mapcar #'pp-tree-printer (children tree)))
         (children-next-line (mapcar (lambda (p) (funcall p nil))
                                     children-printers))
         (children-height-left (mapcar (lambda (w)
                                         (1- (* w pretty-tree-height-mult)))
                                       (edge-weights tree)))
         (children-width (mapcar #'pp-tree-width (children tree)))
         (children-total-width (+ (reduce #'+ children-width)
                                  (* (1- (length children-width))
                                     pretty-tree-horiz-space))))
    (lambda (stream)
      (let (output)
        (cond
          (tree-label                   ; print label line
           (setf output
                 (pp-tree-label-line tree-label tree-width children-width))
           (setf tree-label nil)
           (format stream output))
          ((some (lambda (s) (string/= s "")) children-next-line)
           (setf output (make-array tree-width :element-type 'character
                                    :fill-pointer 0 :initial-element #\Space))
           ;; initial padding for long label
           (incf (fill-pointer output)
                 (ceiling (/ (- tree-width children-total-width) 2)))
           (loop                        ; print children
              for cprinter on children-printers
              for cnext-line on children-next-line
              for cheight-left on children-height-left
              for cwidth on children-width
              for halfway = (/ (1- (car cwidth)) 2)
              do
                (cond
                  ((> (car cheight-left) 0) ; vertical
                   (incf (fill-pointer output) (ceiling halfway))
                   (format output "~C" pretty-tree-vert-char)
                   (incf (fill-pointer output) (floor halfway))
                   (decf (car cheight-left)))
                  ((string/= (car cnext-line) "") ; subtree
                   (format output (car cnext-line))
                   (setf (car cnext-line) (funcall (car cprinter) nil)))
                  (t                    ; nothing
                   (incf (fill-pointer output) (car cwidth))))
              ;; spacing
                (when (cdr cprinter)
                  (incf (fill-pointer output) pretty-tree-horiz-space)))
           (format stream output))
          (t                            ; print nothing
           (format stream "")))))))

(defmethod pp-tree-printer (tree)
  (let ((line (format nil "~A" tree)))
    (lambda (stream)
      (let ((output line))
        (when line
          (setf line ""))
        (format stream output)))))

(defgeneric leafset (tree))

(defmethod leafset ((tree tree))
  (if (consp (children tree))
      (reduce #'nconc (mapcar #'leafset (children tree)))
      (list (label tree))))

(defmethod leafset (tree)
  (list tree))

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

(defgeneric tree-height (tree))

(defmethod tree-height ((tree tree))
  (loop
     for child in (children tree)
     for edge-weight in (edge-weights tree)
     maximizing (+ (tree-height child) edge-weight)))

(defmethod tree-height (tree)
  0)

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

(defun range (beg end &optional (step 1))
  "Returns range of numbers between beg and end."
  (assert (and (<= beg end) (> step 0)))
  (loop for i from beg to end by step collecting i))

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
         (setf x (append x (list (make-instance 'tree :children (list a b)
                                                :edge-weights
                                                (list
                                                 (- h (tree-height a))
                                                 (- h (tree-height b)))))))
         (incf n))
    (car x)))

(defun make-random-tree (x &optional (degree 2))
  "Makes a random tree with leafset X and degree less than or equal to
DEGREE."
  (setf x (remove-duplicates x))
  (let ((n (length x))
        k leaves height)
    (loop while (> n 1) do
         (setf k (random-between 2 (1+ (min n degree))))
         (setf leaves (select-random x k))
         (setf height (1+ (reduce #'max (mapcar #'tree-height leaves))))
         (setf x (set-difference x leaves))
         (setf x (append x (list (make-instance
                                  'tree :children leaves
                                  :edge-weights
                                  (mapcar (lambda (l)
                                            (- height (tree-height l)))
                                          leaves)))))
         (setf n (length x)))
    (car x)))

(defgeneric tree-distances (tree))

(defmethod tree-distances ((tree tree))
  "Returns list of every pairwise distance in tree"
  (let* ((cords (list))
         (h (tree-height tree))
         (children (children tree)))
    (loop for lchild on children do
         (loop for rchild on (rest lchild) do
              (setf cords
                    (append cords
                            (loop for l in (leafset (car lchild)) append
                                 (loop for r in (leafset (car rchild)) collect
                                      (cord l r (* h 2))))))))
    (loop for child in children do
         (setf cords (append cords (tree-distances child))))
    cords))

(defmethod tree-distances (tree)
  nil)

(defgeneric tree-clusters (tree)
  (:documentation "Returns the set of all clusters displayed by this tree."))

(defmethod tree-clusters ((tree tree))
  (cons (leafset tree)
        (reduce #'nconc (mapcar #'tree-clusters (children tree)))))

(defmethod tree-clusters (tree)
  (list (list tree)))

(defun tree-distance (tree1 tree2)
  "The distance between TREE1 and TREE2 according to the Robinson-Foulds
metric."
  (length (set-difference (tree-clusters tree1) (tree-clusters tree2)
                          :test #'equal)))

