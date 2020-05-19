(in-package "DATA-FLOW.COMPONENT")

(defclass base-component ()
  ((%scheduler :initarg :scheduler
               :reader data-flow:scheduler)
   (%event-queue :initarg :event-queue
                 :reader %event-queue)))

(defmethod data-flow:enqueue-event ((component base-component) event)
  (data-flow.queue:enqueue (%event-queue component) event)
  (when (data-flow:compare-and-change-execution-state component :stopped :scheduled)
    (data-flow:schedule (data-flow:scheduler component)
                        (data-flow:make-component-lambda component))))

(defmethod data-flow:process-all-events ((component base-component))
  (data-flow.queue:doqueue (event (%event-queue component))
    (data-flow:process-event component event)))

(defmethod data-flow:requires-execution-p or ((component base-component))
  (not (data-flow.queue:emptyp (%event-queue component))))
