(in-package "DATA-FLOW.QUEUE")

(defclass queue ()
  ())

(defgeneric enqueue (queue item))
(defgeneric dequeue (queue)
  (:documentation "Returns (VALUES OBJECT T) from QUEUE if the queue
  has an object available. Otherwise (VALUES NIL NIL)."))
