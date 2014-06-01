(in-package :gk-trees)

(defun main (argv)
  (when (< (length argv) 2)
    (format t "Usage: ~A matrix_file~%" (first argv))
    (return-from main 1))
  (let ((filename (second argv)))
    (pp-tree-print
     (peek (ultrametric-lasso4
            (csv-to-cords filename
                          :delimiter #\,
                          :labelled t))))))

