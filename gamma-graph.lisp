;;; stuff for dealing with the Gamma(L) graph

(in-package :gk-trees)

(defun make-gamma-graph (cords &key (test #'eq))
  "CORDS should be a list of cords.  If multiple cords are given with
identical ends but different weights, the weight of the edge in the graph is
undefined."
  (let ((graph (make-hash-table :test test)))
    (flet ((ctest (x y) (funcall test (car x) (car y))))
     (loop for cord in cords do
          (setf (gethash (cord-left cord) graph)
                (remove-duplicates
                 (cons (cons (cord-right cord) (cord-length cord))
                       (gethash (cord-left cord) graph))
                 :test #'ctest))
          (setf (gethash (cord-right cord) graph)
                (remove-duplicates
                 (cons (cons (cord-left cord) (cord-length cord))
                       (gethash (cord-right cord) graph))
                 :test #'ctest))))
    graph))

(defmacro make-graph (&rest edges)
  (let ((cords (loop for e in edges collecting `(cord ',(car e)
                                                      ',(cadr e)
                                                      ,(caddr e)))))
    `(make-gamma-graph (list ,@cords))))

(defun edge-eq (e1 e2 &key (test #'eq))
  (or (and (funcall test (car e1) (car e2))
           (funcall test (cdr e1) (cdr e2)))
      (and (funcall test (car e1) (cdr e2))
           (funcall test (cdr e1) (car e2)))))

(defun print-graph-edges (graph)
  (loop for k being the hash-keys in graph using (hash-value v) do
       (dolist (vn v) (format t "~a - ~a = ~a~%" k (car vn) (cdr vn)))))

(defun gamma-vertices (graph)
  "Returns list of all vertices."
  (let ((vertices (list)))
    (maphash #'(lambda (k v) (declare (ignore v)) (push k vertices)) graph)
    vertices))

(defun gamma-edges (graph)
  "Returns list of edges as cons cells."
  (let ((edges (list)))
    (maphash #'(lambda (k v)
                 (mapc #'(lambda (e) (push (cons k (car e)) edges))
                       v))
             graph)
    (remove-duplicates edges :test #'edge-eq)))

(defun gamma-adjacent-vertices (graph vertex)
  "Returns list of adjacent vertices."
  (mapcar #'car (gethash vertex graph)))

(defun gamma-edge-weight (graph v1 v2)
  "Returns weight of edge {v1, v2}, or nil if no such edge."
  (cdr (find v2 (gethash v1 graph) :key #'car)))

(defun path (v1 v2)
  (list v1 v2))

(defun first-path (graph)
  (let* ((vertices (gamma-vertices graph))
         (first (nth (random (length vertices)) vertices))
         (adjacent (gamma-adjacent-vertices graph first)))
    (path first (nth (random (length adjacent)) adjacent))))

(defun path-start (path)
  (car path))

(defun path-end (path)
  (car (last path)))

(defun path-adjacent (path elt)
  (let ((last nil))
    (loop for nc on path do
         (when (eq (car nc) elt)
           (return (remove-if #'null (list last (cadr nc)))))
         (setf last (car nc)))))

(defun extend-path-start (path elt)
  (cons elt path))

(defun extend-path-end (path elt)
  (append path (list elt)))

(defun extend-path (path elt end)
  "END, 0 = start, 1 = end."
  (if (= end 0)
      (extend-path-start path elt)
      (extend-path-end path elt)))

(defun path-is-hamiltonian (graph path)
  (null (set-difference (gamma-vertices graph) path)))

(defun ham (graph)
  "Tries to a find a Hamiltonian path in the graph."
  (let ((P (list (first-path graph)))
        (k 0))
    (tagbody
     L1
       (when (path-is-hamiltonian graph (nth k P))
         (return-from ham (nth k P)))
       (let ((Q (list (nth k P)))
             (s 0) (tee 0) (deltas (list 0)))
         (loop while (< (nth s deltas) (graph-T graph)) do
          (loop with w = (list (path-start (nth s Q)) (path-end (nth s Q)))
             for i in '(0 1) do
               (loop for x in (set-difference
                               (gamma-adjacent-vertices graph (nth i w))
                               (path-adjacent (nth s Q) (nth i w))) do
                    (format t "x: ~a~%" x)
                    (cond
                      ;; extension
                      ((and (not (member x (nth s Q)))
                            (/= (gamma-edge-weight
                                 graph (nth i w) x)
                                (gamma-edge-weight
                                 graph (nth i w)
                                 (car (path-adjacent (nth s Q)
                                                     (nth i w))))))
                       (format t "extend: ~a~%" (nth s Q))
                       (setf P (append P (list (extend-path (nth s Q) x i))))
                       (incf k)
                       (go L1))
                      ;; cycle extension
                      ((eq x (nth (- 1 i) w))
                       (format t "cycle extend: ~a~%" (nth s Q))
                       (loop for c in (nth s Q) do
                            (let ((adj (set-difference
                                        (gamma-adjacent-vertices graph c)
                                        (nth s Q))))
                              (when (consp adj)
                                (setf P (append 
                                         P (list (cycle-extend (nth s Q)
                                                               c (first adj)))))
                                (incf k)
                                (go L1)))))
                      ;; rotations
                      (t
                       (format t "rotate: ~a~%" (nth s Q))
                       (incf tee)
                       (setf Q (append Q (list (rotate-path (nth s Q) x i))))
                       (setf deltas
                             (append deltas
                                     (list (1+ (nth s deltas)))))))))
              (incf s))))))

(defun cycle-extend (cycle u v)
  "Extend cycle to path using edge (u,v) where u is in cycle and v is not."
  (let* ((path (copy-list cycle))
         (tail (member u path))
         (head (cdr tail)))
    (setf (cdr (last tail)) path)
    (setf (cdr tail) (cons v nil))
    head))

(defun rotate-path-start (path u)
  (let* ((mid (copy-list path))
         (start (loop for e on mid do (when (eq (cadr e) u) (return e))))
         (tail (cdr start)))
    (setf (cdr start) nil)
    (nconc (nreverse mid) tail)))

(defun rotate-path-end (path u)
  (let* ((start (copy-list path))
         (mid (member u start))
         (tail (cdr mid)))
    (setf (cdr mid) (nreverse tail))
    start))

(defun rotate-path (path u end)
  "Return a new path by rotating using u in middle of path and either 0 =
start or 1 = end."
  (if (= end 0)
      (rotate-path-start path u)
      (rotate-path-end path u)))

(defun graph-T (graph)
  "Used as maximum search depth by HAM algorithm."
  (let* ((m (length (gamma-edges graph)))
         (n (length (gamma-vertices graph)))
         (d (* 2 (/ m n))))
    (+ (/ (log n 2) (- (log d 2) (log (log d 2) 2))) 1)))

(defun build-binary-from-path (path graph)
  (let* ((tree (first path))
         (last (first path))
         (current-weight 0)
         )
    (loop for leaf in (rest path) do
         (setf current-weight (/ (gamma-edge-weight graph leaf last) 2))
         (cond ((> current-weight (tree-height tree))
                (setf tree (make-proper-cherry leaf current-weight
                                               tree (- current-weight
                                                       (tree-height tree)))))
               ((< current-weight (tree-height tree))
                ;; search down left side to find largest subtree with height <
                ;; (last will always be leftmost)
                (let ((subtree tree)
                      subtree-height)
                  (loop while (< current-weight
                                 (tree-height (left-child subtree))) do
                       (setf subtree (left-child subtree)))
                  (setf subtree-height (tree-height subtree))
                  (setf (left-child subtree)
                        (make-proper-cherry leaf current-weight
                                            (left-child subtree)
                                            (- current-weight
                                               (tree-height
                                                (left-child subtree)))))
                  (setf (left-edge-weight subtree)
                        (- subtree-height
                           (tree-height (left-child subtree))))))
               (t
                (error "Path is an invalid lasso (equal sequential weights).")))
         (setf last leaf))
    tree))
