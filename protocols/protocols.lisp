(in-package "DATA-FLOW")


;;;; Scheduler State
;;;;
;;;; An object representing the current state of the scheduler after
;;;; completing a SCHEDULE.

(defclass scheduler-state ()
  ())

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

(defclass sequential-scheduler (scheduler)
  ())

(defclass parallel-scheduler (scheduler)
  ())
