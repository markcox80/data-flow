#-data-flow.features:threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error (format nil "~A requires DATA-FLOW.FEATURES:THREADS in *FEATURES*."
                 (or *compile-file-pathname*
                     *load-pathname*))))

(defpackage "DATA-FLOW.BT-MUTEX-QUEUE"
  (:use "COMMON-LISP")
  (:export "BT-MUTEX-QUEUE"
           "MAKE-BT-MUTEX-QUEUE"))
(in-package "DATA-FLOW.BT-MUTEX-QUEUE")

(defclass bt-mutex-queue (data-flow.queue:queue)
  ((%lock
    :initarg :lock
    :initform (bordeaux-threads:make-lock "DATA-FLOW.BT-MUTEX-QUEUE::BT-MUTEX-QUEUE")
    :reader lock)
   (%inner-queue
    :initarg :inner-queue
    :reader inner-queue)))

(defun make-bt-mutex-queue (inner-queue)
  (check-type inner-queue data-flow.queue:queue)
  (make-instance 'bt-mutex-queue
                 :inner-queue inner-queue))

(defmethod data-flow.queue:enqueue ((queue bt-mutex-queue) item)
  (bordeaux-threads:with-lock-held ((lock queue))
    (data-flow.queue:enqueue (inner-queue queue) item)))

(defmethod data-flow.queue:dequeue ((queue bt-mutex-queue))
  (bordeaux-threads:with-lock-held ((lock queue))
    (data-flow.queue:dequeue (inner-queue queue))))

(defmethod data-flow.queue:emptyp ((queue bt-mutex-queue))
  (bordeaux-threads:with-lock-held ((lock queue))
    (data-flow.queue:emptyp (inner-queue queue))))
