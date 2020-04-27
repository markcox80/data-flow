
;;;; QUEUE

(in-package "DATA-FLOW.QUEUE")

(defclass queue ()
  ())

(defgeneric emptyp (queue))
(defgeneric enqueue (queue item))
(defgeneric dequeue (queue)
  (:documentation "Returns (VALUES OBJECT T) from QUEUE if the queue
  has an object available. Otherwise (VALUES NIL NIL)."))

(defmacro doqueue ((var queue) &body body)
  (alexandria:with-gensyms (object? queue-var start-tag end-tag)
    `(let* ((,queue-var ,queue)
            ,var ,object?)
       (tagbody
          ,start-tag
          (multiple-value-setq (,var ,object?) (data-flow.queue:dequeue ,queue-var))
          (unless ,object?
            (go ,end-tag))
          ,@body
          (go ,start-tag)
          ,end-tag))))
