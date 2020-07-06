(defpackage "DATA-FLOW.SEQUENTIAL-SCHEDULER"
  (:use "COMMON-LISP")
  (:export "SEQUENTIAL-SCHEDULER"
           "MAKE-SEQUENTIAL-SCHEDULER"))

#+data-flow.features:threads
(defpackage "DATA-FLOW.RESOURCE-SCHEDULER"
  (:use "COMMON-LISP")

  (:export "*DEFAULT-POLL-SECONDS*"
           "RESOURCE-SCHEDULER"
           "MAKE-RESOURCE-SCHEDULER"
           "RESOURCES"

           "INVALID-RESOURCE-REQUIREMENT-ERROR"))

(defpackage "DATA-FLOW.THREAD-POOL"
  (:use "COMMON-LISP")

  (:export "*DEFAULT-POLL-SECONDS*"
           "THREAD-POOL"
           "MAKE-THREAD-POOL"))
