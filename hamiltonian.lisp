;;; finding hamiltonian paths in graphs

(in-package :gk-trees)

(defmacro for-each-end (path &body body)
  "Repeats BODY twice with THIS-END and THAT-END bound to each end of the
path"
  `(let ((this-end (path-start ,path))
         (that-end (path-end ,path)))
     ,@body
     (setf this-end (path-end ,path) that-end (path-start ,path))
     ,@body))

(defun only (list)
  (assert (endp (cdr list)))
  (car list))

(defun path-can-extend-p (path path-end new-vertex graph)
  "Returns true if PATH can be extended at PATH-END by NEW-VERTEX"
  (and (not (member new-vertex path))
       (/= (gamma-edge-weight graph path-end new-vertex)
           (gamma-edge-weight graph path-end
                              (only (path-adjacent path path-end))))))

(defun cycle-can-extend-p (cycle graph)
  "Returns true if it is possible to perform cycle extension on CYCLE"
  (let* ((start (path-start cycle))
         (end (path-end cycle))
         (midw (gamma-edge-weight graph start end)))
    (and midw
         (/= midw
             (gamma-edge-weight graph start
                                (only (path-adjacent cycle start))))
         (/= midw
             (gamma-edge-weight graph end
                                (only (path-adjacent cycle end)))))))

(defun find-cycle-extension (cycle graph)
  "Returns a valid cycle extension or NIL if none exist"
  (loop for vertex in cycle do
       (dbg :fce "vertex: ~a~%" vertex)
       (loop for adj in (set-difference (gamma-adjacent-vertices graph vertex)
                                        (cycle-adjacent cycle vertex)) do
            (dbg :fce "adj: ~a~%" adj)
            (cond ((/= (gamma-edge-weight graph vertex adj)
                       (gamma-edge-weight
                        graph vertex (first (cycle-adjacent cycle vertex))))
                   (return-from find-cycle-extension
                     (cycle-extend2 cycle vertex
                                    (second (cycle-adjacent cycle vertex))
                                    adj)))
                  ((/= (gamma-edge-weight graph vertex adj)
                       (gamma-edge-weight
                        graph vertex (second (cycle-adjacent cycle vertex))))
                   (return-from find-cycle-extension
                     (cycle-extend2 cycle vertex
                                    (first (cycle-adjacent cycle vertex))
                                    adj))))))
  nil)

(defun cycle-extend2 (cycle path-vertex path-adjacent new-vertex)
  "Returns a path by cycle extending CYCLE by breaking cycle between
  PATH-VERTEX and PATH-ADJACENT and adding NEW-VERTEX"
  (let ((path (copy-list cycle))
        rest)
   (loop for vertexc on path do
        (cond ((eq (car vertexc) path-vertex)
               (if (eq (cadr vertexc) path-adjacent)
                   (progn
                     (setf rest (rest vertexc))
                     (setf (cdr vertexc) nil)
                     (return (nconc rest
                                    path
                                    (list new-vertex))))
                   (return (nconc (list new-vertex) path))))
              ((eq (car vertexc) path-adjacent)
               (if (eq (cadr vertexc) path-vertex)
                   (progn
                     (setf rest (rest vertexc))
                     (setf (cdr vertexc) nil)
                     (return (nconc (list new-vertex)
                                    rest
                                    path)))
                   (return (nconc path (list new-vertex)))))))))

(defun cycle-edge-swap (cycle end)
  "Makes a path by removing edge incident with END that is on the path"
  (assert (or (eq end (path-start cycle)) (eq end (path-end cycle))))
  (let ((path (copy-list cycle)))
    (if (eq end (path-start cycle))
        (nconc (cdr path) (list (car path)))
        (nconc (list end) (remove end path :test #'eq)))))

(defun path-can-rotate-p (path end middle graph)
  (assert (member end path))
  (and (member middle path)
       (let ((mid-adjacent (path-adjacent path middle)))
         (if (eq end (path-start path))
             (setf mid-adjacent (second mid-adjacent))
             (setf mid-adjacent (first mid-adjacent)))
         (and (/= (gamma-edge-weight graph end middle)
                  (gamma-edge-weight graph middle mid-adjacent))
              (/= (gamma-edge-weight graph end middle)
                  (gamma-edge-weight
                   graph end (only (path-adjacent path end))))))))

(defun extend-path2 (path elt end)
  (assert (member end path))
  (if (eq end (path-start path))
      (extend-path-start path elt)
      (extend-path-end path elt)))

(defun rotate-path2 (path middle end)
  "Return a new path by rotating using u in middle of path"
  (assert (member end path))
  (assert (member middle path))
  (if (eq end (path-start path))
      (rotate-path-start path middle)
      (rotate-path-end path middle)))

(defun ham2 (graph)
  (let ((paths (make-array 0 :fill-pointer 0 :adjustable t))
        (k 0)
        (deltas (make-array 0 :fill-pointer 0 :adjustable t)))
    (vector-push-extend (first-path graph) paths)
    (vector-push-extend (cons 0 0) deltas)
    (loop while (and (< k (length paths))
                     (< (car (elt deltas k)) (graph-T graph)))
       for path = (elt paths k) do
         (block path-try-extend
           (for-each-end path
             (loop for x in (set-difference
                             (gamma-adjacent-vertices graph this-end)
                             (path-adjacent path this-end)) do
                  (dbg :ham "path: ~a~%x: ~a~%" path x)
                  (cond
                    ;; extension
                    ((not (member x path))
                     (when (path-can-extend-p path this-end x graph)
                       (dbg :ham "extension...~%")
                       (setf k 0
                             (fill-pointer paths) 0
                             (fill-pointer deltas) 0)
                       (vector-push-extend (extend-path2 path x this-end)
                                           paths)
                       (vector-push-extend (cons 0 0) deltas)
                       (dbg :ham "extended: ~a~%" (elt paths 0))
                       (return-from path-try-extend)))
                    ;; cycle extension
                    ((and (eq x that-end))
                     (dbg :ham "cycle...~%")
                     (if (cycle-can-extend-p path graph)
                         (let ((extension (find-cycle-extension path graph)))
                           (dbg :ham "cycle extending...~%")
                           (when extension
                             (setf k 0
                                   (fill-pointer paths) 0
                                   (fill-pointer deltas) 0)
                             (vector-push-extend extension paths)
                             (vector-push-extend (cons 0 0) deltas)
                             (dbg :ham "cycle extended: ~a~%" (elt paths 0))
                             (return-from path-try-extend)))
                         (when (zerop (cdr (elt deltas k)))
                           (dbg :ham "edge swapping...~%")
                           (if (= (gamma-edge-weight graph this-end that-end)
                                  (gamma-edge-weight graph this-end
                                                     (only (path-adjacent
                                                            path this-end))))
                               (vector-push-extend
                                (cycle-edge-swap path this-end)
                                paths)
                               (vector-push-extend
                                (cycle-edge-swap path that-end)
                                paths))
                           (vector-push-extend (cons (car (elt deltas k)) 1)
                                               deltas))))
                    ;; rotation
                    ((path-can-rotate-p path this-end x graph)
                     (dbg :ham "rotating...~%")
                     (vector-push-extend (rotate-path2 path x this-end) paths)
                     (vector-push-extend (cons (1+ (car (elt deltas k))) 0)
                                         deltas)))))
           (incf k)))
    (elt paths 0)))

