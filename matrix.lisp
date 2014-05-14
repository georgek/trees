(in-package :gk-trees)
;;; since these are distance matrices ie. the main diagonal is all zero and it
;;; is symmetric about the diagonal, we can store the matrix in a compact form
;;; inside a one dimensional array. This saves memory, of course, increases
;;; overhead on element lookups, but saves time on insertion since we don't
;;; need to do everything twice. Also traversing an entire row/column is done
;;; as quickly as normal (by recalculating the index into the real array) and
;;; there is no need to traverse both the row and column if updating the
;;; matrix.

;;; for square matrices
(defclass matrix ()
  ((names
    :initarg :names
    :initform nil
    :accessor names
    :documentation "List of names of columns.")
   (values
    :initarg :vals
    :initform nil
    :accessor vals
    :documentation "The matrix.")))

(defmethod print-object ((matrix matrix) stream)
  (print-unreadable-object (matrix stream :type t)
    (format stream "size: ~A"
            (length (names matrix)))))

(defun make-matrix (names)
  (let ((l (length names)))
   (make-instance 'matrix :names names
                  :vals (make-array (list (/ (* l (1- l)) 2))
                                    :initial-element nil))))

(defun copy-matrix (matrix)
  (make-instance 'matrix
                 :vals (copy-seq(vals matrix))
                 :names (copy-list (names matrix))))

(defun ref-to-vref (i j)
  (when (> i j)
    (rotatef i j))
  (+ (/ (* j (1- j)) 2) i))

;; (defun matrix-elt (matrix i j &key (test #'eql))
;;   "Get element by name of column and row."
;;   (aref (vals matrix)
;;         (position i (names matrix) :test test)
;;         (position j (names matrix) :test test)))

(defun matrix-elt (matrix i j)
  (if (= i j)
      0
      (aref (vals matrix) (ref-to-vref i j))))

;; (defun (setf matrix-elt) (val matrix i j &key (test #'eql))
;;   (setf (aref (vals matrix)
;;               (position i (names matrix) :test test)
;;               (position j (names matrix) :test test))
;;         val))

(defun (setf matrix-elt) (val matrix i j)
  (when (/= i j)
    (setf (aref (vals matrix) (ref-to-vref i j))
          val)))

(defun print-matrix (matrix stream &key (delimiter #\,))
  (loop for cname on (names matrix) do
       (format stream "~A" (car cname))
       (unless (endp (cdr cname))
         (format stream "~C" delimiter)))
  (fresh-line stream)
  (let ((dim (1- (length (names matrix)))))
   (loop for i from 0 to dim do
        (loop for j from 0 to dim
           for val = (matrix-elt matrix i j) do
             (if val
                 (format stream "~A" val)
                 (format stream "-"))
             (unless (= j dim)
               (format stream "~C" delimiter)))
        (fresh-line stream))))

(defun cords-to-matrix (cords &optional (leafmap #'identity))
  (let* ((names (cords-vertices cords))
         (matrix (make-matrix (mapcar leafmap names))))
    (setf (names matrix) (sort (names matrix) #'string<))
    (setf (names matrix) (stable-sort (names matrix) #'< :key #'length))
    ;; fill in values from cords
    (loop for cord in cords do
         (setf (matrix-elt matrix
                           (funcall leafmap (cord-left cord))
                           (funcall leafmap (cord-right cord))
                           :test #'equal)
               (cord-length cord))
         (setf (matrix-elt matrix
                           (funcall leafmap (cord-right cord))
                           (funcall leafmap (cord-left cord))
                           :test #'equal)
               (cord-length cord)))
    matrix))

(defun sub-matrix (matrix names &key (test #'eql))
  (let ((sub-matrix (make-matrix names))
        (positions (positions names (names matrix) :test test)))
    (loop for i in positions
       for newi from 0 do
         (loop for j in positions
            for newj from 0 do
              (setf (aref (vals sub-matrix) newi newj)
                    (aref (vals matrix) i j))))
    sub-matrix))

(defun matrix-factor (matrix1 matrix2)
  "Divides elements of each matrix with each other."
  (let ((factors (make-matrix (names matrix1))))
    (loop for i from 0 below (array-dimension (vals matrix1) 0) do
         (loop for j from 0 below (array-dimension (vals matrix2) 1) do
              (if (= 0 (aref (vals matrix2) i j))
                  (setf (aref (vals factors) i j) 0)
                  (setf (aref (vals factors) i j)
                        (/ (aref (vals matrix1) i j)
                           (aref (vals matrix2) i j))))))
    factors))

(defun matrix-to-list (matrix)
  (loop for i from 0 below (array-dimension (vals matrix) 0) append
       (loop for j from 0 below i collect
            (aref (vals matrix) i j))))

(defun csv-to-matrix (filename &key (limit most-positive-fixnum)
                                 (delimiter #\,) (labelled nil)
                                 (truncate-labels nil)
                                 (multiplier 1))
  "Makes a matrix from csv file."
  (let ((matrix nil)
        (names nil))
    (with-open-file (filein filename)
      (loop for line = (read-line filein nil)
         for i from 0 below limit
         with split
         while line do
           (setf split (split-string line delimiter))
           (unless matrix
             ;; set stuff up with first line
             (if labelled
                 (setf names (subseq split 0 (min limit (length split))))
                 (setf names (range 1 (min limit (length split)))))
             (when truncate-labels
               (setf names (mapcar (lambda (s) (subseq s 0 (min (length s) truncate-labels)))
                                   names)))
             (setf matrix (make-matrix names))
             (setf split (split-string (read-line filein) delimiter)))
           (loop for distance in split
              for j from 0 below limit do
                (setf (aref (vals matrix) i j)
                      (* (read-from-string distance)
                         multiplier)))))
    matrix))

(defun matrix-to-cords (matrix)
  "Returns unique cords for matrix."
  (assert (eq (type-of matrix) 'matrix))
  (let ((cords (list)))
    (loop for i from 0 below (length (names matrix)) do
         (loop for j from 0 below i do
              (push (cord (elt (names matrix) i)
                          (elt (names matrix) j)
                          (aref (vals matrix) i j))
                    cords)))
    cords))

