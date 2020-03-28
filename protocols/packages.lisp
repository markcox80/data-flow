(defpackage "DATA-FLOW"
  (:use "COMMON-LISP")

  ;; Runnable protocol
  (:export "RUN")

  ;; Scheduler protocol
  (:export "SCHEDULER"
           "SCHEDULE"
           "EXECUTE"
           "EXECUTE1"
           "EXECUTE-UNTIL"
           "EXECUTINGP"
           "BLOCKING-ALLOWED-P"

           "PARALLEL-SCHEDULER"
           "SEQUENTIAL-SCHEDULER"))
