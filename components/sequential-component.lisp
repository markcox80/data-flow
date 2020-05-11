(in-package "DATA-FLOW.COMPONENT")

(defclass sequential-component (base-component)
  ((%execution-state :initarg :execution-state
                     :initform :stopped
                     :type data-flow:execution-state
                     :reader data-flow:execution-state
                     :accessor %execution-state))
  (:default-initargs
   :event-queue (data-flow.fifo:make-fifo)))

(defmethod data-flow:compare-and-change-execution-state ((component sequential-component) old-state new-state)
  (check-type old-state data-flow:execution-state)
  (check-type new-state data-flow:execution-state)
  (when (eql (%execution-state component) old-state)
    (setf (%execution-state component) new-state)
    t))
