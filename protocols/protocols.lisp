(in-package "DATA-FLOW")


;;;; Scheduler State
;;;;
;;;; An object representing the current state of the scheduler after
;;;; completing a SCHEDULE.

(defclass scheduler-state ()
  ())

;; Return the number of runnables that will be queued for execution
;; the next time the scheduler is started.
(defgeneric count-queued-runnables (scheduler-state))

;; Return the number of runnables left to be executed.
(defgeneric count-remaining-runnables (scheduler-state))

;;;; Scheduler

(defgeneric run (runnable))

(defmethod run ((function function))
  (funcall function))

(defclass scheduler ()
  ())

;; The schedule generic function must return an instance of
;; SCHEDULER-STATE.
(defgeneric schedule (scheduler runnable &key &allow-other-keys))

;; Return T if there are no runnables waiting to be executed. Users
;; should be careful with this predicate in cases where they are using
;; START1 to start the scheduler e.g. a runnable may be blocked
;; waiting for activity from a runnable that is queued.
(defgeneric blocking-allowed-p (scheduler))

(defgeneric executingp (scheduler))

;; Start executing tasks that have been scheduled prior to invoking
;; START1 i.e. any new tasks which are scheduled after calling START1
;; will be postponed until the next time the scheduler starts.
;;
;; Return T if the scheduler was started. NIL if the scheduler was
;; already executing.
(defgeneric start1 (scheduler))

;; Execute tasks that have been scheduled prior to invoking START. All
;; new tasks will be scheduled for execution provided that no task has
;; signalled an error.
(defgeneric start (scheduler))

;; Execute all tasks until no tasks are left or a single task has
;; signalled an error.
(defgeneric wait-until-finished (scheduler &key seconds &allow-other-keys))

;; Delete any resources associated with the scheduler. Users are
;; allowed to start the scheduler after cleaning.
(defgeneric cleanup (scheduler))

;; START1, WAIT-UNTIL-FINISHED and CLEANUP.
(defgeneric execute1 (scheduler))

;; START, WAIT-UNTIL-FINISHED and CLEANUP.
(defgeneric execute (scheduler))


;;;; Sequential Scheduler

(defclass sequential-scheduler (scheduler)
  ())


;;;; Parallel Scheduler

(defclass parallel-scheduler (scheduler)
  ())

(defgeneric number-of-threads (parallel-scheduler))

(defgeneric threads (parallel-scheduler))

;;;; Default implementations for SCHEDULER.

(defmethod execute1 ((scheduler scheduler))
  (start1 scheduler)
  (unwind-protect (wait-until-finished scheduler)
    (cleanup scheduler)))

(defmethod execute ((scheduler scheduler))
  (start scheduler)
  (unwind-protect (wait-until-finished scheduler)
    (cleanup scheduler)))


;;;; Component
;;
;; Encapsulates a component running in parallel with other components.
;;
;; A function F(X) in the component protocol can be either concurrent
;; or non-concurrent with respect to a component X. A non-concurrent
;; function requires that at most one application of F(X) can be
;; executing at any given moment. A concurrent function permits any
;; number of parallel applications of F(X). All concurrent functions
;; are linearisable.

(deftype execution-state ()
  '(member :stopped :scheduled :running))

;; Return the execution state of the component.
;;
;; This is a concurrent function.
(defgeneric execution-state (component))

;; Return T if the execution state of COMPONENT can be changed from
;; OLD-STATE to NEW-STATE.
;;
;; This is a concurrent function.
(defgeneric compare-and-change-execution-state (component old-state new-state))

;; The scheduler to use when the component requires execution.
;;
;; This is a non-concurrent function.
(defgeneric scheduler (component))

;; Add an event to the component's event queue.
;;
;; The component is scheduled for execution if the execution-state can
;; be changed from :stopped to :scheduled.
;;
;; This is a concurrent function.
(defgeneric enqueue-event (component event))

;; Process all of the events in the event queue.
;;
;; Default implementation pulls events from the event queue and
;; processes them with PROCESS-EVENT.
;;
;; This is a non-concurrent function.
(defgeneric process-all-events (component))

;; Process a single event.
;;
;; This is a non-concurrent function.
(defgeneric process-event (component event))

;; Return T if the component has events which have not been processed
;; by the component.
;;
;; This is a non-concurrent function.
(defgeneric requires-execution-p (component)
  (:method-combination or))

;; Perform the action of the component.
;;
;; The following must be executed immediately before executing a
;; component's RUN method:
;;
;;   1. The execution state of component X is changed from
;;      :SCHEDULED to :EXECUTING.
;;
;;   2. PROCESS-ALL-EVENTS is applied to component X.
;;
;; The execution state of component X is changed from :EXECUTING to
;; :STOPPED on completion of a component's RUN method.
;;
;; If REQUIRES-EXECUTION-P returns T for component X then the
;; component is rescheduled for execution and its execution state is
;; changed from :STOPPED to :SCHEDULED.
;;
;; REQUIRES-EXECUTION-P must return NIL if no events were received by
;; the component during execution of a component's RUN method.
;;
;; This is a non-concurrent function.
;; (defgeneric run (component))

;; This class is defined else where due to the way it is implemented.
;; (defclass component ()
;;   ())

(defun make-component-lambda (component)
  (lambda ()
    (assert (compare-and-change-execution-state component :scheduled :running))

    (process-all-events component)
    (unwind-protect (run component)
      (assert (compare-and-change-execution-state component :running :stopped)))

    (when (and (requires-execution-p component)
               (compare-and-change-execution-state component :stopped :scheduled))
      (schedule (scheduler component)
                (make-component-lambda component)))))
