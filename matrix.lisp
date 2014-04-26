(in-package :gk-trees)

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

(defun make-matrix (names)
  (make-instance 'matrix :names names
                 :vals (make-array `(,(length names) ,(length names))
                                   :initial-element nil)))

(defun matrix-elt (matrix i j &key (test #'eql))
  "Get element by name of column and row."
  (aref (vals matrix)
        (position i (names matrix) :test test)
        (position j (names matrix) :test test)))

(defun (setf matrix-elt) (val matrix i j &key (test #'eql))
  (setf (aref (vals matrix)
              (position i (names matrix) :test test)
              (position j (names matrix) :test test))
        val))

(defun print-matrix (matrix stream &key (delimiter #\,))
  (loop for cname on (names matrix) do
       (format stream "~A" (car cname))
       (unless (endp (cdr cname))
         (format stream "~C" delimiter)))
  (fresh-line stream)
  (let ((ilim (1- (array-dimension (vals matrix) 0)))
        (jlim (1- (array-dimension (vals matrix) 1))))
   (loop for i from 0 to ilim do
        (loop for j from 0 to jlim
           for val = (aref (vals matrix) i j) do
             (if val
                 (format stream "~A" val)
                 (format stream "-"))
             (unless (= j jlim)
               (format stream "~C" delimiter)))
        (fresh-line stream))))

(defun cords-to-matrix (cords &optional (leafmap #'identity))
  (let* ((names (cords-vertices cords))
         (matrix (make-matrix (mapcar leafmap names))))
    ;; fill in identity diagonal
    (loop for i from 0 below (length names) do
         (setf (aref (vals matrix) i i) 0))
    ;; fill in values from cords
    (loop for cord in cords do
         (setf (aref (vals matrix)
                     (position (cord-left cord) names :test #'eq)
                     (position (cord-right cord) names :test #'eq))
               (cord-length cord))
         (setf (aref (vals matrix)
                     (position (cord-right cord) names :test #'eq)
                     (position (cord-left cord) names :test #'eq))
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
                                 (truncate-labels nil))
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
                (setf (aref (vals matrix) i j) (read-from-string distance)))))
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

