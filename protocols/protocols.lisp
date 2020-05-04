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

(defgeneric run (scheduler runnable))

(defmethod run (scheduler (function function))
  (funcall function scheduler))

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
