(in-package "DATA-FLOW.COMPONENT")

(defun make-basic-component-event-queue ()
  #+data-flow.features:threads
  (data-flow.bt-mutex-queue:make-srmw-bt-mutex-queue (data-flow.fifo:make-fifo))
  #-data-flow.features:threads
  (data-flow.fifo:make-fifo))

(defclass basic-component (data-flow:component
                           data-flow.sequential-object:sequential-object)
  ((%scheduler :initarg :scheduler
               :reader data-flow:scheduler)
   (%event-queue :initarg :event-queue
                 :initform (make-basic-component-event-queue)
                 :reader %event-queue)
   (%execution-state :initarg :execution-state
                     :initform :stopped
                     :accessor %execution-state)))

(defmethod data-flow:execution-state ((component basic-component))
  (data-flow.sequential-object:linearize component
    (%execution-state component)))

(defmethod data-flow:compare-and-change-execution-state ((component basic-component) old-state new-state)
  (check-type old-state data-flow:execution-state)
  (check-type new-state data-flow:execution-state)
  (data-flow.sequential-object:linearize component
    (when (eql old-state (%execution-state component))
      (setf (%execution-state component) new-state)
      t)))

(defmethod data-flow:enqueue-event ((component basic-component) event)
  (data-flow.queue:enqueue (%event-queue component) event)
  (when (data-flow:compare-and-change-execution-state component :stopped :scheduled)
    (data-flow:schedule (data-flow:scheduler component)
                        (data-flow:make-component-lambda component))))

(defmethod data-flow:process-all-events ((component basic-component))
  (data-flow.queue:doqueue (event (%event-queue component))
    (data-flow:process-event component event)))

(defmethod data-flow:requires-execution-p or ((component basic-component))
  (not (data-flow.queue:emptyp (%event-queue component))))

(defmethod data-flow:make-component-lambda ((component basic-component))
  (lambda ()
    (assert (data-flow:compare-and-change-execution-state component :scheduled :running))

    (let ((required? (unwind-protect (progn
                                       (data-flow:run component)
                                       (data-flow:requires-execution-p component))
                       (assert (data-flow:compare-and-change-execution-state component :running :stopped)))))

      (when (and required?
                 (data-flow:compare-and-change-execution-state component :stopped :scheduled))
        (data-flow:schedule (data-flow:scheduler component)
                            (data-flow:make-component-lambda component))))))

(defmethod data-flow:run :before ((component basic-component))
  (data-flow:process-all-events component))

(defmethod data-flow:run ((component basic-component))
  (declare (ignore component)))
