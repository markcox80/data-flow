(in-package "DATA-FLOW.SEQUENTIAL-OBJECT")


;;;; Task

(defgeneric task-finished-p (task))
(defgeneric task-function (task))
(defgeneric mark-task-completed (task values error))

(defclass task ()
  ((%function :initarg :function
              :reader task-function)))

;;;; Parallel Sequential Object

(deftype parallel-sequential-object-state ()
  '(member :stopped :running))

(defgeneric task-queue (parallel-sequential-object))
(defgeneric compare-and-set-state (parallel-sequential-object current-state new-state))
(defgeneric make-task (parallel-sequential-object function))

(defclass parallel-sequential-object ()
  ())

(defmethod linearized-apply ((function function) (sequential-object parallel-sequential-object))
  (loop
    with task = (make-task sequential-object function)
    with values = nil
    with valuesp = nil
    with error = nil

    initially
       (data-flow.queue:enqueue (task-queue sequential-object) task)

    until valuesp
    do
       (when (compare-and-set-state sequential-object :stopped :running)
         (unwind-protect
              (data-flow.queue:doqueue (task (task-queue sequential-object))
                (handler-case (mark-task-completed task
                                                   (multiple-value-list (funcall (task-function task)))
                                                   nil)
                  (error (condition)
                    (mark-task-completed task nil condition))))
           (assert (compare-and-set-state sequential-object :running :stopped))))

       (multiple-value-setq (values valuesp error) (task-finished-p task))

    finally
       (if error
           (error error)
           (return (values-list values)))))
