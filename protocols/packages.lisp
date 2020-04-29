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
           "SEQUENTIAL-SCHEDULER"))
