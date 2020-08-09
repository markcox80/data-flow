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

           "EXECUTION-ERROR"
           "EXECUTION-ERROR-RUNNABLE"
           "EXECUTION-ERROR-CONDITION"
           "EXECUTION-ERROR-SCHEDULER"

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
           "STANDARD-COMPONENT")

  ;; Port protocol
  (:export "PORT"
           "PORTP"
           "DISCONNECT-PORT"

           "PORT-ERROR"
           "PORT-ERROR-PORT"
           "PORT-DISCONNECTED-ERROR")

  ;; Input Port protocol
  (:export "INPUT-PORT"
           "INPUT-PORT-P"
           "MAKE-INPUT-PORT"
           "READ-VALUE"
           "NO-VALUE-AVAILABLE-ERROR")

  ;; Output Port protocol
  (:export "OUTPUT-PORT"
           "OUTPUT-PORT-P"
           "MAKE-OUTPUT-PORT"
           "WRITE-VALUE"
           "SPACE-AVAILABLE-P"
           "AVAILABLE-SPACE"
           "TOTAL-SPACE"
           "*DEFAULT-TOTAL-SPACE*"
           "NO-SPACE-AVAILABLE-ERROR")

  ;; Connection protocol
  (:export "ALREADY-CONNECTED-ERROR"

           "INPUT-PORT"
           "INPUT-COMPONENT"
           "OUTPUT-PORT"
           "OUTPUT-COMPONENT"

           "CONNECTION"
           "CONNECTEDP"

           "CONNECT-PORTS")

  ;; Port utilities
  (:export "READ-VALUE-CASE"
           "SUCCESS"
           "NO-VALUE-AVAILABLE"
           "DISCONNECTED"

           "WRITE-VALUE-CASE"
           "SUCCESS"
           "NO-SPACE-AVAILABLE"
           "DISCONNECTED"))
