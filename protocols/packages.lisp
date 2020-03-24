(defpackage "DATA-FLOW"
  (:use "COMMON-LISP")

  ;; Scheduler protocol
  (:export "SCHEDULER"
           "SCHEDULE"
           "EXECUTE"
           "EXECUTE1"
           "EXECUTE-UNTIL"
           "EXECUTINGP"
           "BLOCKING-ALLOWED-P"))
