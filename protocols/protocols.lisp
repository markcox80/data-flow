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

(define-condition execution-error (error)
  ((%scheduler :initarg :scheduler
               :reader execution-error-scheduler)
   (%runnable :initarg :runnable
              :reader execution-error-runnable)
   (%condition :initarg :condition
               :reader execution-error-condition))
  (:documentation "Any unhandled errors by a runnable are wrapped in a EXECUTION-ERROR condition.")
  (:report (lambda (condition stream)
             (pprint-logical-block (stream nil)
               (format stream "Unhandled error encountered by scheduler ~A.~%~%"
                       (execution-error-scheduler condition))
               (pprint-logical-block (stream nil :per-line-prefix "  ")
                 (print-object (execution-error-condition condition) stream))
               (pprint-newline :mandatory stream)
               (pprint-newline :mandatory stream)
               (pprint-logical-block (stream nil)
                 (write-string "Runnable: " stream)
                 (pprint-indent :block 2 stream)
                 (pprint-newline :linear stream)
                 (print-object (execution-error-runnable condition) stream))))))

(defvar *on-error* :start1
  "This variable indicates how a scheduler handles an error signalled
by a runnable during an invocation of DATA-FLOW:RUN.

A value of :START1 indicates that the error should be caught and the
scheduler transitions to a state as if it were started using START1.

A value of :DEBUG indicates that CL:INVOKE-DEBUGGER should be called.

A value of :IGNORE indicates that the error should be ignored.

A value of :WARN-AND-IGNORE indicates that a message should be printed
to *DEBUG-IO* and the error should be ignored.

A value of :WARN-AND-START1 indicates that a message should be printed
to *DEBUG-IO* and the scheduler should proceed as if the value of this
variable were START1.
")

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

;; Set the scheduler to use when the component requires execution.
;;
;; This is a non-concurrent function.
(defgeneric (setf scheduler) (scheduler component))

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

;; Return a function object which performs the action of the
;; component.
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

(defgeneric make-component-lambda (component))

(defclass component ()
  ())

(defvar *scheduler* nil
  "The scheduler used by all components if none is specified.")

;; The BASIC-COMPONENT class provides an implementation of the
;; component protocol. It has no support for ports.
;;
;; This class is defined elsewhere due to the way it is implemented.
;; (defclass basic-component (component)
;;   ())

;; The STANDARD-COMPONENT class is a subclass of BASIC-COMPONENT. It
;; includes support for ports.
;;
;; This class is defined elsewhere due to the way it is implemented.
;; (defclass standard-component (basic-component)
;;   ())

;;;; Port protocol

(defgeneric portp (port))

(defgeneric disconnect-port (port))
(defgeneric connection (port))
(defgeneric connectedp (port))

(defclass port ()
  ())

(define-condition port-error ()
  ((%port :initarg :port
          :reader port-error-port)))

(define-condition port-disconnected-error (port-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unable to perform operation as the port ~A is disconnected."
                     (port-error-port condition)))))

(defmethod portp (port)
  (declare (ignore port))
  nil)

(defmethod portp ((port port))
  (declare (ignore port))
  t)

(defmethod connectedp ((port port))
  (not (null (connection port))))

;;;; Input port protocol

(defgeneric read-value (port &key errorp no-value-value disconnected-value &allow-other-keys))
(defgeneric input-port-p (port))

;; Defined elsewhere due to implementation.
;; (defun make-input-port ())

(defclass input-port (port)
  ())

(define-condition no-value-available-error (port-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No value available on port ~A."
                     (port-error-port condition)))))

(defmethod input-port-p (port)
  (declare (ignore port))
  nil)

(defmethod input-port-p ((port input-port))
  (declare (ignore port))
  t)

;;;; Output port protocol

(defgeneric write-value (value port &key errorp no-space-value disconnected-value &allow-other-keys))

(defvar *default-total-space* 10)
(defgeneric space-available-p (output-port))
(defgeneric available-space (output-port))
(defgeneric total-space (output-port))
(defgeneric output-port-p (port))

;; Defined elsewhere due to implementation.
;; (defun make-output-port ())

(define-condition no-space-available-error (port-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No space available on connection for port ~A."
                     (port-error-port condition)))))

(defclass output-port (port)
  ())

(defmethod output-port-p (port)
  (declare (ignore port))
  nil)

(defmethod output-port-p ((port output-port))
  (declare (ignore port))
  t)

(defmethod space-available-p ((port output-port))
  (plusp (available-space port)))

;;;; Connection protocol

(define-condition already-connected-error (port-error)
  ()
  (:report (lambda (condition stream)
             (format stream "The port ~A is already connected."
                     (port-error-port condition)))))

(defgeneric input-port (connection))
(defgeneric input-component (connection))
(defgeneric output-port (connection))
(defgeneric output-component (connection))

(defclass connection ()
  ())

;; This is a concurrent function with respect to the components.
(defgeneric connect-ports (component1 port1 component2 port2 &key &allow-other-keys))
