(in-package :gk-trees)

(defparameter pretty-tree-horiz-space 1) ; this should be at least 0
(defparameter pretty-tree-height-mult 2) ; this should be at least 1
(defparameter pretty-tree-vertical-space 1)  ; this should be at least 0
(defparameter pretty-tree-width-mult 2) ; this should be at least 1

;;; unicode set
(defparameter pretty-tree-horiz-char #\─)
(defparameter pretty-tree-vert-char #\│)
(defparameter pretty-tree-vert-end-char #\│)
(defparameter pretty-tree-left-corner-char #\╭)
(defparameter pretty-tree-right-corner-char #\╮)
(defparameter pretty-tree-top-corner-char #\╭)
(defparameter pretty-tree-bottom-corner-char #\╰)
(defparameter pretty-tree-down-char #\┬)
(defparameter pretty-tree-out-char #\├)
(defparameter pretty-tree-hnode-char #\┤)
(defparameter pretty-tree-vnode-char #\┴)
(defparameter pretty-tree-cross-char #\┼)

;;; ascii set
;; (defconst pretty-tree-horiz-char #\-)
;; (defconst pretty-tree-vert-char #\|)
;; (defconst pretty-tree-vert-end-char #\|)
;; (defconst pretty-tree-left-corner-char #\+)
;; (defconst pretty-tree-right-corner-char #\+)
;; (defconst pretty-tree-node-char #\^)

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

;;; methods for other lisp objects
(defmethod label (tree)
  (format nil "~A" tree))

(defmethod children (tree)
  nil)

(defmethod edge-weights (tree)
  nil)

(defun treep (object)
  (typep object 'tree))

(defun make-tree (tree)
  "Makes a tree from a Lisp tree."
  (if (consp tree)
      (make-instance 'tree :children (mapcar #'make-tree tree))
      (make-instance 'tree :label tree)))

(defun make-tree-phylip (string &key (truncate-labels nil) (index nil))
  "Makes a tree from output from phylip.  If an index is given then the labels
should be integers which will be translated to the nth (beginning with 1)
value in the index."
  (let ((current-tree (list (make-tree nil)))
        (current-string (make-array 10 :element-type 'character
                                    :fill-pointer 0 :adjustable t)))
    (loop for c across string do
         (case c
           (#\( (push (make-tree nil) current-tree)
                (push (car current-tree) (children (cadr current-tree))))
           (#\) (push (read-from-string current-string)
                      (edge-weights (car current-tree)))
                (when (> (fill-pointer current-string) 0)
                  (setf (fill-pointer current-string) 0)
                  (pop current-tree)))
           (#\, (when (> (fill-pointer current-string) 0)
                  (push (read-from-string current-string)
                        (edge-weights (car current-tree)))
                  (setf (fill-pointer current-string) 0)))
           (#\: (when (> (fill-pointer current-string) 0)
                  (let ((label (copy-seq current-string)))
                    (when index
                      (dbg :mktrph "index~%")
                      (setf label (nth (1- (parse-integer label)) index)))
                    (when truncate-labels
                      (setf label (subseq label 0 (min (length label) truncate-labels))))
                    (push (reduce (lambda (s1 s2) (concatenate 'string s1 "_" s2))
                                  (split-string label #\_))
                          (children (car current-tree)))
                    (setf (fill-pointer current-string) 0))))
           (#\Tab nil)
           (#\Newline nil)
           (#\; nil)
           (t   (vector-push-extend c current-string))))
    (car (children (car current-tree)))))

(defun tree-print (tree &optional (stream t))
  (print-tree-phylip tree stream)
  (format stream ";"))

(defgeneric print-tree-phylip (tree stream)
  (:documentation "Prints tree in Newick format."))

(defmethod print-tree-phylip ((tree tree) stream)
  (when (consp (children tree))
    (format stream "(")
    (loop for cc on (children tree)
       for ec on (edge-weights tree)
       do
         (print-tree-phylip (car cc) stream)
         (format stream ":~f" (car ec))
         (unless (endp (cdr cc))
           (format stream ",")))
    (format stream ")"))
  (when (label tree)
    (format stream "~a" (label tree))))

(defmethod print-tree-phylip (tree stream)
  (format stream "~A" tree))

(defmethod print-object ((object tree) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "with ~A leaves" (length (leafset object)))))

;;; TODO: This seems to work but it would be good to prove that it's correct
(defun pp-tree-width-multiplier (tree width)
  "Calculates the width multiplier necessary to ensure TREE is no more than
WIDTH wide when pretty printing horizontally."
  (if (= (tree-height tree) 0)
      1
      (let* ((printer (pp-tree-hprinter tree))
             (original-width (tree-height tree))
             (multiplier (/ width original-width))
             actual-width)
        ;; now work out actual width when using that multiplier
        (let ((pretty-tree-width-mult multiplier))
          (setf actual-width
                (loop repeat (pp-tree-h-height tree)
                   maximize (length (funcall printer nil)))))
        (floor (/ (- (* 2 width) actual-width) original-width)))))

(defun pp-tree-print (tree &key (stream t) (vertical nil) (width 80))
  (if vertical
      (let* ((printer (pp-tree-printer tree))
             (next-line (funcall printer nil)))
        (loop while (string/= next-line "") do
             (format stream "~A~%" next-line)
             (setf next-line (funcall printer nil))))
      (let ((printer (pp-tree-hprinter tree))
            (pretty-tree-width-mult (pp-tree-width-multiplier tree width)))
        (loop repeat (pp-tree-h-height tree) do
             (funcall printer stream)
             (format stream "~%")))))

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

(defgeneric pp-tree-h-height (tree))

(defmethod pp-tree-h-height ((tree tree))
  (let ((nleaves (length (leafset tree))))
    (+ nleaves (* (1- nleaves) pretty-tree-vertical-space))))

(defmethod pp-tree-h-height (tree)
  1)

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
                         (string pretty-tree-vnode-char)))
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

(defun round-to-one (val)
  (if (= 0 val)
      (values 0 0)
      (let ((rnd (max 1 (round val))))
        (values rnd (- val rnd)))))

(defvar *rounded-off-amount* 0
  "The amount rounded off in drawing the branch to this tree.")

(defgeneric pp-tree-hprinter (tree)
  (:documentation "Returns a closure like pp-tree-printer but for printing
  horizontally."))

(defmethod pp-tree-hprinter ((tree tree))
  (assert (>= (length (edge-weights tree)) (length (children tree))))
  (if (consp (children tree))
      (let* ((tree-label (if (label tree) (format nil "~A" (label tree))
                             nil))
             (tree-height (pp-tree-h-height tree))
             (tree-height-left tree-height)
             (children-printers (mapcar #'pp-tree-hprinter (children tree)))
             (edge-weights (edge-weights tree))
             (children-heights (mapcar #'pp-tree-h-height (children tree)))
             (children-heights-left (copy-list children-heights))
             (space-left 0)
             (state :before))
        (lambda (stream)
          (let ((output (make-array 80 :element-type 'character
                                    :fill-pointer 0 :initial-element #\Space)))
            (multiple-value-bind (edge-length *rounded-off-amount*)
                (round-to-one (+ (* (car edge-weights)
                                    pretty-tree-width-mult)
                                 *rounded-off-amount*))
              ;; label line
              (cond
                ((= tree-height-left (ceiling (/ tree-height 2)))
                 ;; print the label
                 (when (or (> (car edge-weights) 0)
                           (> (car children-heights) 1)
                           (> space-left 0))
                   (if tree-label
                       (format output (if children-printers
                                          (subseq tree-label 0 1)
                                          tree-label))
                       (if (and (= space-left 0)
                                (= (car children-heights-left)
                                   (ceiling (/ (car children-heights) 2))))
                           (format output (string pretty-tree-cross-char))
                           (format output (string pretty-tree-hnode-char))))))
                ((> space-left 0)
                 (format output (string pretty-tree-vert-char)))
                ((= (car children-heights-left)
                    (ceiling (/ (car children-heights) 2)))
                 (if (or (> (car edge-weights) 0)
                         (> (car children-heights) 1))
                     (case state
                       (:before (format output (string pretty-tree-top-corner-char))
                                (setf state :during))
                       (:during (if (cdr children-heights)
                                    (format output (string pretty-tree-out-char))
                                    (progn
                                      (format output
                                              (string pretty-tree-bottom-corner-char))
                                      (setf state :after)))))))
                ((eq state :during)
                 (format output (string pretty-tree-vert-char)))
                (t
                 (format output " ")))
              ;; children
              (if (> space-left 0)
                  (decf space-left)
                  (when children-printers
                    ;; edge
                    (loop repeat (1- edge-length) do
                         (if (= (car children-heights-left)
                                (ceiling (/ (car children-heights) 2)))
                             (format output "~c" pretty-tree-horiz-char)
                             (incf (fill-pointer output))))
                    ;; child
                    (funcall (car children-printers) output)

                    (decf (car children-heights-left))
                    (when (= 0 (car children-heights-left))
                      (pop children-printers)
                      (pop edge-weights)
                      (pop children-heights)
                      (pop children-heights-left)
                      (setf space-left pretty-tree-vertical-space)))))
            (decf tree-height-left)
            (format stream output))))
      ;; tree has no children
      (pp-tree-hprinter (label tree))))

(defmethod pp-tree-hprinter (tree)
  (let ((line (format nil "~A" tree)))
    (lambda (stream)
      (let ((output line))
        (when line
          (setf line ""))
        (format stream output)))))

(defmethod pp-tree-height (tree)
  (pp-tree-printer tree))

(defun vector-push-extend-vector (new-elements vector)
  (loop for item across new-elements do
       (vector-push-extend item vector))
  vector)

(defun texify-string (string)
  (let ((new-string (make-array (length string) :element-type 'character
                                :fill-pointer 0 :adjustable t)))
    (loop for character across string do
         (cond ((find character "&%$#_{}")
                (vector-push-extend #\\ new-string)
                (vector-push-extend character new-string))
               ((char= #\~ character)
                (vector-push-extend-vector "\\textasciitilde" new-string))
               ((char= #\^ character)
                (vector-push-extend-vector "\\textasciicircum" new-string))
               ((char= #\\ character)
                (vector-push-extend-vector "\\textbackslash" new-string))
               (t
                (vector-push-extend character new-string))))
    new-string))

(defparameter tikz-tree-print-root-width 0.02
  "The width of the root where 1 is the width of the entire tree.")

(defgeneric tikz-tree-print (tree &optional x y label output)
  (:documentation "Prints TikZ output for drawing a tree with (La)TeX. The
  optional parameters should be left as default."))

(defmethod tikz-tree-print ((tree tree)
                            &optional (x 0) (y 0) (label "r") (output t))
  (if (consp (children tree))
      (progn
        ;; this tree's root
        (format output "\\node (~A) at (~F,~F) {};~%" label x y)
        (when (= x 0)
          (format output "\\node (rr) at (~F,~F) {};~%"
                  (- (* (tree-height tree) tikz-tree-print-root-width))
                  y)
          (format output "\\draw (~A.center) -- (rr.center);~%" label))
        (loop with y = (- y (/ (1- (length (leafset tree))) 2))
           for child in (children tree)
           for i from 1
           for child-label = (format nil "~A~D" label i)
           for child-height in (edge-weights tree)
           for child-width = (1- (length (leafset child)))
           do
             ;; print each child
             (tikz-tree-print child
                              (+ x child-height)
                              (+ y (/ child-width 2))
                              child-label
                              output)
             ;; join root to child's root
             (format output "\\draw (~A.center) |- (~A.center);~%"
                     label child-label)
             (incf y (1+ child-width))))
      ;; tree is a leaf, print label
      (format output "\\node[label=right:{~A}] (~A) at (~F,~F) {};~%"
              (texify-string (format nil "~A" (label tree))) label x y)))

(defmethod tikz-tree-print (tree &optional (x 0) (y 0) (label "r") (output t))
  (format output "\\node[label=right:{$~A$}] (~A) at (~F,~F) {};~%"
          (texify-string (format nil "~A" tree)) label x y))

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

(defun nth-child (n tree)
  (assert (< n (length (children tree))))
  (nth n (children tree)))

(defun nth-edge-weight (n tree)
  (assert (< n (length (edge-weights tree))))
  (nth n (edge-weights tree)))

(defun last-child (tree)
  (car (last (children tree))))

(defun last-edge-weight (tree)
  (car (last (edge-weights tree))))

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
                 pretty-tree-vnode-char) ;node
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
                 pretty-tree-vnode-char)
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
  "Makes a random binary tree with leafset X using the Yule-Harding model"
  (setf x (mapcar #'make-tree (remove-duplicates x)))
  (let* ((leaves (select-random x 2))
         (tree (make-instance 'tree :children leaves))
         (rest (set-difference x leaves)))
    (loop while (consp rest)
       for new-leaf = (car (select-random rest 1))
       for old-leaf = (car (select-random leaves 1))
       for new-old-leaf = (make-tree (label old-leaf))
       do
         (setf rest (remove new-leaf rest :test #'eq))
         (setf leaves (remove old-leaf leaves :test #'eq))
         (push new-leaf leaves)
         (push new-old-leaf leaves)
         (setf (label old-leaf) nil)
         (setf (children old-leaf) (list new-leaf new-old-leaf))
         (setf (edge-weights old-leaf) (list 1 1)))
    tree))

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

(defun sets-equal (list1 list2)
  (null (set-exclusive-or list1 list2 :test #'equal)))

(defun tree-distance (tree1 tree2)
  "The distance between TREE1 and TREE2 according to the Robinson-Foulds
metric."
  (length (set-difference (tree-clusters tree1) (tree-clusters tree2)
                          :test #'sets-equal)))

(defun tree-make-ultrametric (tree)
  (let ((height (tree-height tree)))
    (make-instance 'tree
                   :label (label tree)
                   :children (mapcar #'tree-make-ultrametric (children tree))
                   :edge-weights (mapcar (lambda (c) (- height (tree-height c)))
                                         (children tree)))))

(defgeneric canonical< (object1 object2))

(defmethod canonical< (object1 object2)
  t)

(defmethod canonical< ((object1 number) (object2 number))
  (< object1 object2))

(defmethod canonical< ((object1 number) object2)
  t)

(defmethod canonical< (object1 (object2 number))
  nil)

(defmethod canonical< ((object1 string) (object2 string))
  (string< object1 object2))

(defmethod canonical< ((object1 string) (object2 tree))
  (if (consp (children object2))
      t
      (canonical< object1 (label object2))))

(defmethod canonical< ((object1 tree) (object2 string))
  (if (consp (children object1))
      nil
      (canonical< (label object1) object2)))

(defmethod canonical< ((object1 list) object2)
  nil)

(defmethod canonical< (object1 (object2 list))
  t)

(defmethod canonical< ((object1 tree) (object2 tree))
  (if (consp (children object1))
      (if (consp (children object2))
          (canonical< (car (children object1)) (car (children object2)))
          nil)
      (if (consp (children object2))
          t
          (canonical< (label object1) (label object2)))))

(defgeneric canonicalise-tree (tree)
  (:documentation "Sorts the leaves."))

(defmethod canonicalise-tree ((tree tree))
  (mapc #'canonicalise-tree (children tree))
  (let ((c-ew (sort (mapcar #'cons (children tree) (edge-weights tree))
                    #'canonical< :key #'car)))
    (setf (children tree) (mapcar #'car c-ew))
    (setf (edge-weights tree) (mapcar #'cdr c-ew)))
  tree)

(defmethod canonicalise-tree (tree)
  tree)

(defgeneric all-edge-weights (tree))

(defmethod all-edge-weights ((tree tree))
  (if (consp (edge-weights tree))
      (append (edge-weights tree)
              (reduce #'append (mapcar #'all-edge-weights (children tree))))
      (list)))

(defmethod all-edge-weights (tree)
  (list))

(defun tree-subtree (tree leafset)
  "Returns the subtree of TREE over the given LEAFSET."
  (let ((subtree (tree-subtree-untrimmed tree leafset)))
    (if (= (length (children subtree)) 1)
        (nth-child 0 subtree)
        subtree)))

;;; this actually does subtree calculation but can return a tree with a degree
;;; 1 root which needs to be trimmed
(defun tree-subtree-untrimmed (tree leafset)
  (if (null (children tree))
      (car (member tree leafset))
      (let ((children (list))
            (edge-weights (list))
            subtree)
        (loop for child in (children tree)
           for edge-weight in (edge-weights tree)
           do
             (when (intersection leafset (leafset child) :test #'equal)
               (push child children)
               (push edge-weight edge-weights)))
        (setf subtree (make-instance 'tree
                                     :children (mapcar (lambda (c)
                                                         (tree-subtree-untrimmed c leafset))
                                                       children)
                                     :edge-weights edge-weights :label (label tree)))
        ;; remove degree one vertices in children
        (loop for childc on (children subtree)
           for edge-weightc on (edge-weights subtree)
           do
             (when (= (length (children (car childc))) 1)
               (setf (car edge-weightc) (+ (car edge-weightc)
                                           (nth-edge-weight 0 (car childc))))
               (setf (car childc) (nth-child 0 (car childc)))))
        subtree)))

