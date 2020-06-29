#-data-flow.features:compare-and-set
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:compare-and-set-unavailable-error))

(in-package "DATA-FLOW.SEQUENTIAL-OBJECT")

(defclass cas-task (task)
  ((%values :initarg :values
            :initform nil)
   (%valuep :initarg :valuesp
             :initform nil)
   (%error :initarg :error
           :initform nil)))

(defmethod task-finished-p ((task cas-task))
  (with-slots (%values %valuep %error) task
    (let* ((valuep %valuep)
           (values %values)
           (error %error)
           (valuep2 %valuep))
      (when (and (eql valuep valuep2)
                 (not (null valuep)))
        (values values valuep error)))))

(defmethod mark-task-completed ((task cas-task) values error)
  (with-slots (%values %valuep %error) task
    (setf %values values
          %error error)
    (data-flow.utils:compare-and-set %valuep nil t))
  (values))

(defclass cas-sequential-object (parallel-sequential-object)
  ((%queue :initarg :queue
           :initform (data-flow.lock-free-fifo:make-lock-free-fifo)
           :reader task-queue)
   (%state :initarg :state
           :initform :stopped)))

(defmethod make-task ((cas cas-sequential-object) function)
  (make-instance 'cas-task :function function))

(defmethod compare-and-set-state ((cas cas-sequential-object) current-state new-state)
  (check-type current-state parallel-sequential-object-state)
  (check-type new-state parallel-sequential-object-state)
  (with-slots (%state) cas
    (data-flow.utils:compare-and-set %state current-state new-state)))
