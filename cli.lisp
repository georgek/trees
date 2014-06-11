(in-package :gk-trees)

(defvar *program-name* "lasso")

(defun usage ()
  (format *standard-output*
          "Usage: ~A [-n | -p | -t | -T] matrix_file~%" *program-name*)
  1)

(defun main (argv)
  (let ((*program-name* (first argv))
        (argv (rest argv)))
   (unless (consp argv)
     (return-from main (usage)))
   (handler-case
       (let* ((output-mode :newick)
              (filename nil)
              cords forest)
         (loop for arg in argv do
              (if (char= (aref arg 0) #\-)
                  (handler-case
                      (setf output-mode
                            (ecase (aref arg 1)
                              (#\n :newick)
                              (#\p :pretty)
                              (#\t :tikz)
                              (#\T :tikz-polar)))
                    (type-error (err)
                      (format *error-output* "Unrecognised argument: ~A~%"
                              (type-error-datum err))))
                  (setf filename arg)))
         (when (null filename)
           (return-from main (usage)))
         (setf cords (csv-to-cords filename :delimiter #\, :labelled t))
         (setf forest (ultrametric-lasso4 cords))
         (case output-mode
           (:pretty
            (pp-tree-print (canonicalise-tree (peek forest)) :stream *standard-output*))
           (:tikz
            (tikz-tree-print (canonicalise-tree (peek forest)) *standard-output*))
           (:tikz-polar
            (tikz-tree-print-polar (canonicalise-tree (peek forest)) *standard-output*))
           (otherwise
            (tree-print (peek forest) *standard-output*))))
     (condition (err)
       (format *error-output* "~A: ~%  ~S~%"
               (class-name (class-of err)) err)))))

