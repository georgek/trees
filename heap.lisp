;;; heaps

(in-package :gk-trees)

(defclass queue ()
  ((comparison
    :initarg :comparison
    :initform #'>
    :documentation "A binary comparison operator which should return true if
    the LHS has higher priority than the RHS.")
   (key
    :initarg :key
    :initform #'identity
    :documentation "Function to call to get key from value with which to
    compare with other objects in heap.")
   (array
    :initarg :initial-contents
    :initform (make-array 20 :fill-pointer 0 :adjustable t)
    :documentation "A sequence of objects.")))

(defmethod initialize-instance :after ((queue queue) &key)
  (with-slots (array) queue
    (let ((length (length array))
          new-array)
     (when (plusp length)
       (setf new-array (make-array (* length 2) :fill-pointer 0 :adjustable t))
       (map-into new-array #'identity array)
       (setf array new-array)
       (heapify-array queue)))))

(defmethod print-object ((object queue) stream)
  (with-slots (comparison key array) object
    (let ((comp-details (multiple-value-list
                        (function-lambda-expression comparison)))
          (key-details (multiple-value-list
                        (function-lambda-expression key))))
     (print-unreadable-object (object stream :type t)
       (format stream "Order: ~A. Key: ~A. Size: ~A."
               (or (nth 2 comp-details) comparison)
               (or (nth 2 key-details) key)
               (length array))))))

(defun enqueue (object queue)
  "Puts object in queue."
  (with-slots (comparison key array) queue
    (let ((index (vector-push-extend object array)))
      (sift-up queue index))))

(defun dequeue (queue)
  "Dequeues and returns top of queue (index 0)."
  (with-slots (comparison key array) queue
    (case (length array)
      (0 nil)
      (1 (vector-pop array))
      (t (let ((top (aref array 0)))
           (setf (aref array 0) (vector-pop array))
           (sift-down queue 0)
           top)))))

(defun dequeue-top-to-list (queue)
  "Dequeues and returns all elements with priority equal to top of queue."
  (with-slots (comparison key array) queue
    (when (plusp (length array))
      (let ((top-value (funcall key (aref array 0))))
        (loop until (queue-empty-p queue)
           while (= top-value (funcall key (aref array 0)))
           collect (dequeue queue))))))

(defun dequeue-at (queue index)
  "Dequeues and returns object at given index in queue."
  (with-slots (comparison key array) queue
    (let ((removed (aref array index)))
      ;; move last element in array to that position
      (setf (aref array index) (vector-pop array))
      (if (and (plusp index)
               (funcall comparison
                        (funcall key (aref array index))
                        (funcall key (aref array (pa index)))))
          (sift-up queue index)
          (sift-down queue index))
      removed)))

(defun peek (queue)
  (with-slots (array) queue
    (when (plusp (length array))
      (aref array 0))))

(defun empty-queue (queue)
  (with-slots (array) queue
    (setf (fill-pointer array) 0)))

(defun queue-size (queue)
  (with-slots (array) queue
    (length array)))

(defun queue-empty-p (queue)
  (with-slots (array) queue
    (zerop (length array))))

;;; indexing functions
(defun fc (index)
  "Returns index of first child."
  (+ (* index 2) 1))

(defun sc (index)
  "Returns index of second child."
  (+ (* index 2) 2))

(defun pa (index)
  "Returns index of parent."
  (floor (/ (- index 1) 2)))

(defun sift-down (queue index)
  (declare (type fixnum index))
  (with-slots (array key comparison) queue
   (loop with largest = index do
        (when (and (< (fc index) (length array))
                   (funcall comparison
                            (funcall key (aref array (fc index)))
                            (funcall key (aref array index))))
          (setf largest (fc index)))
        (when (and (< (sc index) (length array))
                   (funcall comparison
                            (funcall key (aref array (sc index)))
                            (funcall key (aref array largest))))
          (setf largest (sc index)))
        (if (not (= index largest))
            (progn
              (rotatef (aref array index)
                       (aref array largest))
              (setf index largest))
            (return)))))

(defun sift-up (queue index)
  (declare (type fixnum index))
  (with-slots (array key comparison) queue
    (loop while (and (> index 0)
                     (funcall comparison 
                              (funcall key (aref array index)) 
                              (funcall key (aref array (pa index)))))
       do
       ;; swap with parent
         (rotatef (aref array index)
                  (aref array (pa index)))
         (setf index (pa index)))))

(defun heapify-array (queue)
  (with-slots (array) queue
   (let ((start (floor (/ (- (length array) 2) 2))))
     (loop for i from start downto 0 do
          (sift-down queue i)))))

