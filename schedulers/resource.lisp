#-data-flow.features:threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:threads-unavailable-error))

(in-package "DATA-FLOW.RESOURCE-SCHEDULER")

;;;; Resource scheduler protocol

(defparameter *exit* '#:exit)
(defgeneric next-runnable (scheduler returning-resources last-error))

;;;; Resource scheduler worker

(defclass worker ()
  ((%scheduler :initarg :scheduler
               :reader scheduler)
   (%thread :initarg :thread
            :accessor thread)))

(defun main-worker-loop (worker)
  (loop
    with scheduler = (scheduler worker)
    with last-resources = nil
    with last-error = nil
    for (runnable resources) = (next-runnable scheduler last-resources last-error)
    until (eql runnable *exit*)
    do
       (setf last-resources resources
             last-error nil)
       (handler-case (data-flow:run scheduler runnable)
         (error (e)
           (setf last-error e)))))

(defun make-worker (scheduler)
  (let* ((worker (make-instance 'worker :scheduler scheduler)))
    (setf (thread worker) (bordeaux-threads:make-thread (lambda ()
                                                          (main-worker-loop worker))
                                                        :name (format nil "~A" 'resource-scheduler)))
    worker))


;;;; Resource Scheduler State
;;;;
;;;; This object is returned by the SCHEDULE method.

(defclass resource-scheduler-state (data-flow:scheduler-state)
  ((%remaining-count :initarg :remaining-count
                     :reader remaining-count
                     :reader data-flow:count-remaining-runnables)
   (%queued-count :initarg :queued-count
                  :reader data-flow:count-queued-runnables)
   (%remaining-resources :initarg :remaining-resources
                         :reader remaining-resources)
   (%state :initarg :state
           :reader state)))

(defun make-resource-scheduler-state (remaining-count queued-count remaining-resources state)
  (make-instance 'resource-scheduler-state
                 :remaining-count remaining-count
                 :queued-count queued-count
                 :remaining-resources remaining-resources
                 :state state))

(defmethod data-flow:executingp ((object resource-scheduler-state))
  (not (eql :paused (state object))))

;;;; Resource Scheduler

(deftype runnable-resources ()
  '(real 0))

(deftype scheduler-state ()
  '(member :paused :executing1 :executing))

(defvar *default-poll-seconds* 0.0001)

(define-condition invalid-resource-requirement-error (error)
  ((%total-resources :initarg :total-resources
                     :reader invalid-resource-requirement-error-total-resources)
   (%required-resources :initarg :required-resources
                        :reader invalid-resource-requirement-error-required-resources)
   (%runnable :initarg :runnable
              :reader invalid-resource-requirement-error-runnable)
   (%scheduler :initarg :scheduler
               :reader invalid-resource-requirement-error-scheduler))
  (:report (lambda (condition stream)
             (format stream "The amount of resource required for ~A exceeds the total number of resources managed by the scheduler ~A i.e. ~A > ~A."
                     (invalid-resource-requirement-error-runnable condition)
                     (invalid-resource-requirement-error-scheduler condition)
                     (invalid-resource-requirement-error-required-resources condition)
                     (invalid-resource-requirement-error-total-resources condition)))))

(defclass resource-scheduler (data-flow:parallel-scheduler)
  ((%resources :initarg :resources
               :reader resources)
   (%number-of-threads :initarg :number-of-threads
                       :reader data-flow:number-of-threads)
   (%poll-seconds :initarg :poll-seconds
                  :initform 0.0001
                  :reader poll-seconds)
   (%wait-lock :initarg :wait-lock
               :initform (bordeaux-threads:make-lock (format nil "~A::WAIT-LOCK" 'resource-scheduler))
               :reader wait-lock)
   (%lock :initarg :lock
          :initform (bordeaux-threads:make-lock (format nil "~A::LOCK" 'resource-scheduler))
          :reader lock)
   (%scheduled-queue :initarg :scheduled-queue
                     :initform (data-flow.fifo:make-fifo)
                     :reader %scheduled-queue)
   (%executing-queue :initarg :executing-queue
                     :initform (data-flow.fifo:make-fifo)
                     :reader %executing-queue)
   (%remaining-resources :initarg :remaining-resources
                         :accessor %remaining-resources)
   (%remaining-count :initarg :remaining-count
                     :initform 0
                     :accessor %remaining-count)
   (%queued-count :initarg :queued-count
                  :initform 0
                  :accessor %queued-count)
   (%state :initarg :state
           :initform :paused
           :accessor %state)
   (%error :initarg :error
           :initform nil
           :accessor %error)
   (%workers :initarg :workers
             :initform nil
             :accessor %workers)
   (%resources-function :initarg :resources-function
                        :initform (constantly 1)
                        :reader %resources-function)))

(defun make-resource-scheduler (number-of-threads
                                &key
                                  (resources number-of-threads)
                                  (resources-function (constantly 1))
                                  (poll-seconds *default-poll-seconds*))
  (check-type resources runnable-resources)
  (check-type number-of-threads (integer 1))
  (make-instance 'resource-scheduler
                 :resources resources
                 :remaining-resources resources
                 :number-of-threads number-of-threads
                 :resources-function resources-function
                 :poll-seconds poll-seconds))

(defmethod data-flow:schedule ((scheduler resource-scheduler) runnable &key required-resources)
  (check-type required-resources (or null runnable-resources))
  (bordeaux-threads:with-lock-held ((lock scheduler))
    (setf required-resources (or required-resources
                                 (funcall (%resources-function scheduler) runnable)))
    (when (> required-resources (resources scheduler))
      (error 'invalid-resource-requirement-error
             :scheduler scheduler
             :runnable runnable
             :total-resources (resources scheduler)
             :required-resources required-resources))
    (data-flow.queue:enqueue (ecase (%state scheduler)
                               ((:paused :executing1)
                                (incf (%queued-count scheduler))
                                (%scheduled-queue scheduler))
                               ((:executing
                                 (incf (%remaining-count scheduler))
                                 (%executing-queue scheduler))))
                             (list runnable required-resources))
    (make-resource-scheduler-state (%remaining-count scheduler)
                                   (%queued-count scheduler)
                                   (%remaining-resources scheduler)
                                   (%state scheduler))))

(defun start-helper (scheduler new-state)
  (check-type new-state (member :executing1 :executing))
  (bordeaux-threads:with-lock-held ((lock scheduler))
    (ecase (%state scheduler)
      (:paused
       (setf (%state scheduler) new-state
             (%error scheduler) nil)
       (data-flow.queue:doqueue (runnable (%scheduled-queue scheduler))
         (incf (%remaining-count scheduler))
         (data-flow.queue:enqueue (%executing-queue scheduler) runnable))
       (setf (%queued-count scheduler) 0)
       (unless (%workers scheduler)
         (setf (%workers scheduler) (loop
                                      for index from 0 below (data-flow:number-of-threads scheduler)
                                      collect
                                      (make-worker scheduler))))
       t)
      (:executing1
       (when (and (eql new-state :executing)
                  (null (%error scheduler)))
         (setf (%state scheduler) :executing)
         t))
      (:executing
       (when (eql new-state :executing1)
         (setf (%state scheduler) :executing1)
         t)))))

(defmethod data-flow:start ((scheduler resource-scheduler))
  (start-helper scheduler :executing))

(defmethod data-flow:start1 ((scheduler resource-scheduler))
  (start-helper scheduler :executing1))

(defmethod data-flow:cleanup ((scheduler resource-scheduler))
  (loop
    with workers = nil
    with finished? = nil
    until finished?
    do
       (setf workers nil)
       (bordeaux-threads:with-lock-held ((lock scheduler))
         (when (eql :paused (%state scheduler))
           (setf workers (%workers scheduler)
                 (%workers scheduler) nil
                 finished? t)
           (when workers
             (dotimes (i (data-flow:number-of-threads scheduler))
               (data-flow.queue:enqueue (%executing-queue scheduler) *exit*)))))
       (unless finished?
         (bordeaux-threads:thread-yield))

    finally
       (dolist (worker workers)
         (bordeaux-threads:join-thread (thread worker))))
  (values))

(defmethod data-flow:wait-until-finished ((scheduler resource-scheduler) &key seconds poll-seconds)
  (check-type seconds (or null (real 0)))
  (check-type poll-seconds (or null (real 0)))
  (loop
    with poll-seconds = (or poll-seconds (poll-seconds scheduler))
    with start = (get-internal-real-time)
    for current = (get-internal-real-time)
    while (or (null seconds)
              (< (/ (- current start)
                    internal-time-units-per-second)
                 seconds))
    do
       (bordeaux-threads:with-lock-held ((lock scheduler))
         (when (eql :paused (%state scheduler))
           (if (%error scheduler)
               (error (%error scheduler))
               (return-from data-flow:wait-until-finished t))))
       (bordeaux-threads:thread-yield)
       (sleep poll-seconds))
  nil)

(defmethod next-runnable ((scheduler resource-scheduler) last-resources last-error)
  (check-type last-resources (or null runnable-resources))
  (check-type last-error (or null error))
  (loop
    with poll-seconds = (poll-seconds scheduler)
    with runnable = nil
    with required-resources = nil
    with loop-state = :poll
    until (member loop-state '(:terminate :run))
    do
       (ecase loop-state
         (:poll
          (with-accessors ((resources resources)
                           (%remaining-resources %remaining-resources)
                           (%remaining-count %remaining-count)
                           (%state %state)
                           (%error %error)
                           (%executing-queue %executing-queue))
              scheduler
            (bordeaux-threads:with-lock-held ((lock scheduler))
              (when (realp last-resources)
                (decf %remaining-count)
                (incf %remaining-resources last-resources)

                ;; This needs to be done in case we loop.
                (setf last-resources nil))

              (when last-error
                (setf %state :executing1
                      %error last-error)

                ;; This needs to be done in case we loop
                (setf last-error nil))

              ;; When no tasks are available, ensure the remaining
              ;; resources equals the total number of resources. This
              ;; avoids any issues with floating point arithmetic.
              (when (zerop %remaining-count)
                (setf %remaining-resources resources
                      %state :paused))

              (check-type %remaining-count (integer 0))
              (check-type %remaining-resources (real 0))

              (multiple-value-bind (data data?) (data-flow.queue:dequeue %executing-queue)
                (setf loop-state (cond ((and data? (eql data *exit*))
                                        :terminate)

                                       (data?
                                        (setf runnable (first data)
                                              required-resources (second data))
                                        (assert (<= required-resources (resources scheduler)))
                                        (cond ((>= %remaining-resources required-resources)
                                               (decf %remaining-resources required-resources)
                                               :run)
                                              (t
                                               :wait-for-resources)))

                                       (t
                                        :wait-for-runnable)))))))

         (:wait-for-resources
          (with-accessors ((wait-lock wait-lock)
                           (lock lock)
                           (%remaining-resources %remaining-resources))
              scheduler
            (sleep poll-seconds)
            ;; The wait lock ensures only one of the waiting threads
            ;; will compete with threads that are finishing a task.
            (bordeaux-threads:with-lock-held (wait-lock)
              (bordeaux-threads:with-lock-held (lock)
                (when (>= %remaining-resources required-resources)
                  (decf %remaining-resources required-resources)
                  (setf loop-state :run))))))

         (:wait-for-runnable
          (sleep poll-seconds)
          (setf loop-state :poll)))

    finally
       (return
         (ecase loop-state
           (:run (list runnable required-resources))
           (:terminate (list *exit* nil))))))

(defmethod data-flow:executingp ((scheduler resource-scheduler))
  (bordeaux-threads:with-lock-held ((lock scheduler))
    (not (eql :paused (%state scheduler)))))

(defmethod data-flow:blocking-allowed-p ((scheduler resource-scheduler))
  (bordeaux-threads:with-lock-held ((lock scheduler))
    (<= (%remaining-count scheduler)
        (data-flow:number-of-threads scheduler))))

(defmethod data-flow:threads ((scheduler resource-scheduler))
  (let* ((workers (bordeaux-threads:with-lock-held ((lock scheduler))
                    (%workers scheduler))))
    (mapcar #'thread workers)))