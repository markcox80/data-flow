#-data-flow.features:threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:threads-unavailable-error))

(in-package "DATA-FLOW.THREAD-POOL")

;;;; Worker

(defparameter *exit* '#:exit)
(defparameter *no-last-error* '#:no-last-error)
(defgeneric next-runnable (scheduler last-error))
(defgeneric worker-scheduler (worker))
(defgeneric worker-thread (worker))

(defclass worker ()
  ((%scheduler :initarg :scheduler
               :reader worker-scheduler)
   (%thread :initarg :thread
            :reader worker-thread)))

(defun make-worker (scheduler)
  (let* ((worker (make-instance 'worker :scheduler scheduler)))
    (with-slots (%thread) worker
      (setf %thread (bordeaux-threads:make-thread (lambda ()
                                                    (main-worker-loop worker))
                                                  :name "DATA-FLOW.THREAD-POOL::WORKER")))
    worker))

(defun main-worker-loop (worker)
  (loop
    with scheduler = (worker-scheduler worker)
    with last-error = *no-last-error*
    for runnable = (next-runnable scheduler last-error)
    until (eql runnable *exit*)
    do
       (setf last-error nil)
       (handler-case (data-flow:run runnable)
         (error (e)
           (setf last-error (make-instance 'data-flow:execution-error
                                           :scheduler scheduler
                                           :condition e
                                           :runnable (original-runnable runnable)))))))


;;;; Parallel Thread Pool

(defgeneric execution-state (object))

(deftype parallel-thread-pool-execution-state ()
  '(member :stopped :executing1 :executing))

(defclass parallel-thread-pool-state (data-flow:scheduler-state)
  ((%remaining-count :initarg :remaining-count
                     :reader data-flow:count-remaining-runnables)
   (%queued-count :initarg :queued-count
                  :reader data-flow:count-queued-runnables)
   (%execution-state :initarg :execution-state
                     :reader execution-state)))

(defun make-parallel-thread-pool-state (remaining-count queued-count execution-state)
  (make-instance 'parallel-thread-pool-state
                 :remaining-count remaining-count
                 :queued-count queued-count
                 :execution-state execution-state))

(defclass parallel-thread-pool (data-flow:parallel-scheduler
                                data-flow.sequential-object:sequential-object)
  ((%number-of-threads :initarg :number-of-threads
                       :initform 0
                       :reader data-flow:number-of-threads)
   (%poll-seconds :initarg :poll-seconds
                  :initform *default-poll-seconds*
                  :reader poll-seconds)
   (%execution-state :initform :stopped
                     :accessor %execution-state)
   (%scheduled-queue :initform (data-flow.fifo:make-fifo)
                     :reader %scheduled-queue)
   (%executable-queue :initform (data-flow.fifo:make-fifo)
                      :reader %executable-queue)
   (%executable-queue-top :initform nil
                          :accessor %executable-queue-top)
   (%remaining-count :initform 0
                     :accessor %remaining-count)
   (%queued-count :initform 0
                  :accessor %queued-count)
   (%workers :initform nil
             :accessor %workers)
   (%error :initform nil
           :accessor %error)))

(defmethod data-flow:threads ((scheduler parallel-thread-pool))
  (data-flow.sequential-object:linearize scheduler
    (mapcar #'worker-thread (%workers scheduler))))

(defmethod data-flow:schedule ((thread-pool parallel-thread-pool) runnable &key &allow-other-keys)
  (data-flow.sequential-object:linearize thread-pool
    (let* ((queue (ecase (%execution-state thread-pool)
                    ((:stopped :executing1)
                     (incf (%queued-count thread-pool))
                     (%scheduled-queue thread-pool))
                    (:executing
                     (incf (%remaining-count thread-pool))
                     (%executable-queue thread-pool)))))
      (data-flow.queue:enqueue queue runnable)
      (make-parallel-thread-pool-state (%remaining-count thread-pool)
                                       (%queued-count thread-pool)
                                       (%execution-state thread-pool)))))

(defun start-helper (thread-pool new-execution-state)
  (check-type thread-pool parallel-thread-pool)
  (check-type new-execution-state (member :executing :executing1))
  (data-flow.sequential-object:linearize thread-pool
    (ecase (%execution-state thread-pool)
      (:stopped
       (assert (zerop (%remaining-count thread-pool)))
       (setf (%execution-state thread-pool) new-execution-state
             (%error thread-pool) nil)

       (let* ((executable-queue (%executable-queue thread-pool)))
         (data-flow.queue:doqueue (runnable (%scheduled-queue thread-pool))
           (data-flow.queue:enqueue executable-queue runnable))
         (setf (%remaining-count thread-pool) (%queued-count thread-pool)
               (%queued-count thread-pool) 0)
         (unless (%workers thread-pool)
           (setf (%workers thread-pool) (loop
                                          for index from 0 below (data-flow:number-of-threads thread-pool)
                                          collect
                                          (make-worker thread-pool)))))
       t)

      (:executing1
       (when (and (eql new-execution-state :executing)
                  (null (%error thread-pool)))
         (setf (%execution-state thread-pool) :executing)
         t))

      (:executing
       (when (eql new-execution-state :executing1)
         (setf (%execution-state thread-pool) :executing1)
         t)))))

(defmethod data-flow:start1 ((thread-pool parallel-thread-pool))
  (start-helper thread-pool :executing1))

(defmethod data-flow:start ((thread-pool parallel-thread-pool))
  (start-helper thread-pool :executing))

(defmethod data-flow:wait-until-finished ((thread-pool parallel-thread-pool) &key seconds poll-seconds)
  (check-type seconds (or null (real 0)))
  (check-type poll-seconds (or null (real 0)))
  (loop
    with finished? = nil
    with poll-seconds = (or poll-seconds (poll-seconds thread-pool))
    with start = (get-internal-real-time)
    for current = (get-internal-real-time)
    while (and (not finished?)
               (or (null seconds)
                   (< (/ (- current start)
                         internal-time-units-per-second)
                      seconds)))
    do
       (setf finished? (data-flow.sequential-object:linearize thread-pool
                         (when (eql (%execution-state thread-pool) :stopped)
                           (assert (zerop (%remaining-count thread-pool)))
                           (let* ((condition (%error thread-pool)))
                             (cond (condition
                                    (setf (%error thread-pool) nil)
                                    (error condition))
                                   (t
                                    t))))))
       (sleep poll-seconds)
    finally
       (return finished?)))

(defmethod data-flow:cleanup ((thread-pool parallel-thread-pool))
  (loop
    with queue = (%executable-queue thread-pool)
    with workers = nil
    with finished? = nil
    until finished?
    do
       (data-flow.sequential-object:linearize thread-pool
         (when (eql (%execution-state thread-pool) :stopped)
           (assert (zerop (%remaining-count thread-pool)))
           (setf workers (%workers thread-pool)
                 (%workers thread-pool) nil
                 finished? t)
           (dolist (worker workers)
             (declare (ignore worker))
             (data-flow.queue:enqueue queue *exit*))))
       (unless finished?
         (sleep (poll-seconds thread-pool)))
    finally
       (dolist (worker workers)
         (bordeaux-threads:join-thread (worker-thread worker)))))

(defun %pop-executable-queue (thread-pool)
  (check-type thread-pool parallel-thread-pool)
  (with-accessors ((top %executable-queue-top)) thread-pool
    (multiple-value-bind (runnable runnable?) (data-flow.queue:dequeue (%executable-queue thread-pool))
      (setf top runnable)
      (values top runnable?))))

(defun %peak-executable-queue (thread-pool)
  (check-type thread-pool parallel-thread-pool)
  (with-accessors ((top %executable-queue-top)) thread-pool
    (cond (top
           (values top t))
          (t
           (%pop-executable-queue thread-pool)))))

(defmethod next-runnable ((thread-pool parallel-thread-pool) last-error)
  ;; Handle the result of the last runnable.
  (unless (eql last-error *no-last-error*)
    (data-flow.sequential-object:linearize thread-pool
      (when (and (null (%error thread-pool))
                 last-error)
        (setf (%error thread-pool) last-error
              (%execution-state thread-pool) :executing1))

      (decf (%remaining-count thread-pool))
      (assert (not (minusp (%remaining-count thread-pool))))))

  ;; Obtain the next runnable.
  (loop
    with poll-seconds of-type real = (poll-seconds thread-pool)
    for runnable = (data-flow.sequential-object:linearize thread-pool
                     (multiple-value-bind (runnable runnable?) (%peak-executable-queue thread-pool)
                       (cond (runnable?
                              (when (execute-runnable-p thread-pool runnable)
                                (%pop-executable-queue thread-pool)
                                runnable))
                             ((zerop (%remaining-count thread-pool))
                              (setf (%execution-state thread-pool) :stopped)
                              nil))))
    until runnable
    do
       (sleep poll-seconds)
    finally
       (return runnable)))

(defmethod data-flow:blocking-allowed-p ((thread-pool parallel-thread-pool))
  (data-flow.sequential-object:linearize thread-pool
    (<= (%remaining-count thread-pool)
        (data-flow:number-of-threads thread-pool))))

(defmethod data-flow:executingp ((thread-pool parallel-thread-pool))
  (data-flow.sequential-object:linearize thread-pool
    (not (eql :stopped (%execution-state thread-pool)))))

(defmethod execute-runnable-p ((thread-pool parallel-thread-pool) runnable)
  (declare (ignore thread-pool runnable))
  t)
