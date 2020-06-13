(defpackage "DATA-FLOW"
  (:use "COMMON-LISP")

  ;; Runnable protocol
  (:export "RUN")

  ;; Scheduler State
  (:export "SCHEDULER-STATE"
           "COUNT-QUEUED-RUNNABLES"
           "COUNT-REMAINING-RUNNABLES")

  ;; Scheduler protocol
  (:export "SCHEDULER"
           "SCHEDULE"
           "EXECUTINGP"
           "START"
           "START1"
           "WAIT-UNTIL-FINISHED"
           "CLEANUP"
           "EXECUTE1"
           "EXECUTE"
           "BLOCKING-ALLOWED-P"

           "PARALLEL-SCHEDULER"
           "NUMBER-OF-THREADS"
           "THREADS"

           "SEQUENTIAL-SCHEDULER")

  ;; Component protocol
  (:export "COMPONENT"
           "EXECUTION-STATE"
           "COMPARE-AND-CHANGE-EXECUTION-STATE"
           "MAKE-COMPONENT-LAMBDA"
           "ENQUEUE-EVENT"
           "PROCESS-ALL-EVENTS"
           "PROCESS-EVENT"
           "REQUIRES-EXECUTION-P"
           "RUN")

  ;; Component protocol implementations
  (:export "BASIC-COMPONENT"
           "STANDARD-COMPONENT"))
