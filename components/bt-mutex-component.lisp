#-data-flow.features:threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:threads-unavailable-error))

(in-package "DATA-FLOW.COMPONENT")

(defclass bt-mutex-component (base-component)
  ((%lock :initarg :lock
          :initform (bordeaux-threads:make-lock "DATA-FLOW.COMPONENT::BT-MUTEX-COMPONENT")
          :reader lock)
   (%execution-state :initarg :execution-state
                     :initform :stopped
                     :type data-flow:execution-state
                     :accessor %execution-state))
  (:default-initargs
   :event-queue (data-flow.bt-mutex-queue:make-bt-mutex-queue
                 (data-flow.fifo:make-fifo))))

(defmethod data-flow:execution-state ((component bt-mutex-component))
  (bordeaux-threads:with-lock-held ((lock component))
    (%execution-state component)))

(defmethod data-flow:compare-and-change-execution-state ((component bt-mutex-component) old-state new-state)
  (with-accessors ((lock lock)
                   (%execution-state %execution-state))
    component
    (bordeaux-threads:with-lock-held (lock)
      (when (eql %execution-state old-state)
        (setf %execution-state new-state)
        t))))
