;;; heaps

(in-package :gk-trees)

(defclass heap ()
  ((order
    :initarg :order
    :initform #'<
    :accessor order
    :documentation "The function used to order the elements.")
   (contents
    :initarg :contents
    :initform (make-array 10 :fill-pointer 0 :adjustable t)
    :accessor contents
    :documentation "The underlying vector.")))

(defun make-heap (&optional (order #'<))
  (make-instance 'heap
                 :order order))

(defmethod print-object ((object heap) stream)
  (with-slots (order contents) object
    (let ((fun-details (multiple-value-list
                        (function-lambda-expression order))))
     (print-unreadable-object (object stream :type t)
       (format stream "Order: ~A. Size: ~A."
               (or (nth 2 fun-details) order)
               (length contents))))))

