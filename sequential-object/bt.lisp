#-data-flow.features:threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:threads-unavailable-error))

(in-package "DATA-FLOW.SEQUENTIAL-OBJECT")

(defclass bt-task (task)
  ((%lock :initarg :lock
          :initform (bordeaux-threads:make-lock "DATA-FLOW.SEQUENTIAL-OBJECT::BT-TASK")
          :reader lock)
   (%values :initarg :values
            :initform nil
            :accessor %values)
   (%valuep :initarg :valuesp
            :initform nil
            :accessor %valuep)
   (%error :initarg :error
           :initform nil
           :accessor %error)))

(defmethod task-finished-p ((task bt-task))
  (bordeaux-threads:with-lock-held ((lock task))
    (values (%values task)
            (%valuep task)
            (%error task))))

(defmethod mark-task-completed ((task bt-task) values error)
  (bordeaux-threads:with-lock-held ((lock task))
    (setf (%values task) values
          (%error task) error
          (%valuep task) t))
  (values))

(defclass bt-sequential-object (parallel-sequential-object)
  ((%btso-queue :initform (data-flow.bt-mutex-queue:make-srmw-bt-mutex-queue (data-flow.fifo:make-fifo))
                :reader task-queue)
   (%btso-lock :initform (bordeaux-threads:make-lock "DATA-FLOW.SEQUENTIAL-OBJECT::BT-SEQUENTIAL-OBJECT")
               :reader lock)
   (%btso-state :initform :stopped
                :accessor %state)))

(defmethod make-task ((bt bt-sequential-object) function)
  (make-instance 'bt-task :function function))

(defmethod compare-and-set-state ((bt bt-sequential-object) current-state new-state)
  (check-type current-state parallel-sequential-object-state)
  (check-type new-state parallel-sequential-object-state)
  (with-accessors ((lock lock)
                   (%state %state))
      bt
    (bordeaux-threads:with-lock-held (lock)
      (when (eql %state current-state)
        (setf %state new-state)
        t))))
