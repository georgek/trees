(in-package :gk-trees)

(defun main (argv)
  (when (< (length argv) 2)
    (format t "Usage: ~A matrix_file~%" (first argv))
    (return-from main 1))
  (handler-case
      (let* ((filename (second argv))
             (cords (csv-to-cords filename
                                  :delimiter #\,
                                  :labelled t))
             (forest (ultrametric-lasso4 cords)))
        (format t "Distances given: ~D~%" (length cords))
        (pp-tree-print (peek forest) :stream t)
        (format t "Distances used: ~D~%"
                (length (lassoed-tree-used-cords (peek forest)))))
    (condition (err)
      (format *error-output* "~A: ~%  ~S~%"
              (class-name (class-of err)) err))))

