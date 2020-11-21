(defpackage "DATA-FLOW.SCHEDULER"
  (:use "COMMON-LISP")
  (:import-from "DATA-FLOW"
                "*ON-ERROR*")
  (:export "*ON-ERROR*"
           "RUN-WITH-ERROR-HANDLING"
           "IGNORE"
           "START1"
           "WARN-AND-IGNORE"
           "WARN-AND-START1")

  (:export "ORIGINAL-RUNNABLE"))

(defpackage "DATA-FLOW.SEQUENTIAL-SCHEDULER"
  (:use "COMMON-LISP")
  (:export "SEQUENTIAL-SCHEDULER"
           "MAKE-SEQUENTIAL-SCHEDULER"))

(defpackage "DATA-FLOW.RESOURCE-SCHEDULER"
  (:use "COMMON-LISP")

  (:export "*DEFAULT-POLL-SECONDS*"
           "RESOURCE-SCHEDULER"
           "MAKE-RESOURCE-SCHEDULER"
           "RESOURCES"

           "INVALID-RESOURCE-REQUIREMENT-ERROR")

  ;; Resource Protocol
  (:export "TEST-RESOURCES-P"
           "TEST-AND-CLAIM-RESOURCES"
           "RETURN-RESOURCES"))

(defpackage "DATA-FLOW.THREAD-POOL"
  (:use "COMMON-LISP")

  (:export "*DEFAULT-POLL-SECONDS*"
           "THREAD-POOL"
           "MAKE-THREAD-POOL"
           "EXECUTE-RUNNABLE-P"))
