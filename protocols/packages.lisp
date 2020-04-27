(defpackage "DATA-FLOW"
  (:use "COMMON-LISP")

  ;; Runnable protocol
  (:export "RUN")

  ;; Scheduler protocol
  (:export "SCHEDULER"
           "SCHEDULE"
           "EXECUTINGP"
           "START"
           "START1"
           "WAIT-UNTIL-FINISHED"
           "CLEANUP"
           "EXECUTE"
           "BLOCKING-ALLOWED-P"

           "PARALLEL-SCHEDULER"
           "SEQUENTIAL-SCHEDULER"))
