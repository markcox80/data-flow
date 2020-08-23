(in-package "DATA-FLOW.SEQUENTIAL-SCHEDULER")

(defclass sequential-scheduler-state (data-flow:scheduler-state)
  ((%remaining-count :initarg :remaining-count
                     :reader data-flow:count-remaining-runnables)
   (%queued-count :initarg :queued-count
                  :reader data-flow:count-queued-runnables)
   (%state :initarg :state
           :reader state)))

(defun make-sequential-scheduler-state (remaining-count queued-count state)
  (make-instance 'sequential-scheduler-state
                 :remaining-count remaining-count
                 :queued-count queued-count
                 :state state))

(defclass sequential-scheduler (data-flow:sequential-scheduler)
  ((%scheduled-queue :initarg :scheduled-queue
                     :initform (data-flow.fifo:make-fifo)
                     :reader scheduled-queue)
   (%executing-queue :initarg :executing-queue
                     :initform (data-flow.fifo:make-fifo)
                     :reader executing-queue)
   (%state :initarg :state
           :initform :paused
           :accessor state)
   (%remaining-count :initarg :remaining-count
                     :initform 0
                     :accessor remaining-count)
   (%queued-count :initarg :scheduled-count
                  :initform 0
                  :accessor queued-count)
   (%error-condition :initarg :error-condition
                     :initform nil
                     :accessor error-condition)))

(defmethod error-condition-p ((scheduler sequential-scheduler))
  (not (null (error-condition scheduler))))

(defun make-sequential-scheduler ()
  (make-instance 'sequential-scheduler))

(defmethod data-flow:schedule ((scheduler sequential-scheduler) runnable &key)
  (data-flow.queue:enqueue (ecase (state scheduler)
                             ((:paused :executing1)
                              (incf (queued-count scheduler))
                              (scheduled-queue scheduler))
                             (:executing
                              (incf (remaining-count scheduler))
                              (executing-queue scheduler)))
                           runnable)
  (make-sequential-scheduler-state (remaining-count scheduler)
                                   (queued-count scheduler)
                                   (state scheduler)))

(defun start-helper (scheduler new-state)
  (check-type new-state (member :executing :executing1))
  (ecase (state scheduler)
    (:paused
     (assert (zerop (remaining-count scheduler)))
     (let* ((executing-queue (executing-queue scheduler)))
       (data-flow.queue:doqueue (runnable (scheduled-queue scheduler))
         (data-flow.queue:enqueue executing-queue runnable)))
     (setf (error-condition scheduler) nil
           (remaining-count scheduler) (queued-count scheduler)
           (queued-count scheduler) 0
           (state scheduler) new-state)
     t)
    (:executing1
     (when (and (eql new-state :executing)
                (not (error-condition-p scheduler)))
       (setf (state scheduler) :executing)
       t))
    (:executing
     (when (eql new-state :executing1)
       (setf (state scheduler) :executing1)
       t))))

(defmethod data-flow:start ((scheduler sequential-scheduler))
  (start-helper scheduler :executing))

(defmethod data-flow:start1 ((scheduler sequential-scheduler))
  (start-helper scheduler :executing1))

(defmethod data-flow:wait-until-finished ((scheduler sequential-scheduler) &key seconds)
  (check-type seconds (or null (real 0)))
  (loop
    with executing-queue = (executing-queue scheduler)
    with start-time = (get-internal-run-time)
    for current-time = (get-internal-run-time)
    while (and (not (data-flow.queue:emptyp executing-queue))
               (or (null seconds)
                   (<= (/ (- current-time start-time)
                          internal-time-units-per-second)
                       seconds)))
    do
       (let* ((runnable (data-flow.queue:dequeue executing-queue)))
         (handler-case (data-flow.scheduler:run-with-error-handling scheduler runnable)
           (error (c)
             (setf (error-condition scheduler) (make-instance 'data-flow:execution-error
                                                              :scheduler scheduler
                                                              :condition c
                                                              :runnable runnable)
                   (state scheduler) :executing1))))
       (decf (remaining-count scheduler)))
  (when (data-flow.queue:emptyp (executing-queue scheduler))
    (setf (state scheduler) :paused)
    (let* ((error-condition (error-condition scheduler)))
      (cond (error-condition
             (setf (error-condition scheduler) nil)
             (error error-condition))
            (t
             t)))))

(defmethod data-flow:cleanup ((scheduler sequential-scheduler))
  (data-flow:wait-until-finished scheduler))

(defmethod data-flow:executingp ((scheduler sequential-scheduler))
  (not (eql (state scheduler) :paused)))

(defmethod data-flow:blocking-allowed-p ((scheduler sequential-scheduler))
  (data-flow.queue:emptyp (executing-queue scheduler)))
