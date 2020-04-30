#+data-flow.features:threads
(defpackage "DATA-FLOW.RESOURCE-SCHEDULER"
  (:use "COMMON-LISP")

  (:export "*DEFAULT-POLL-SECONDS*"
           "RESOURCE-SCHEDULER"
           "MAKE-RESOURCE-SCHEDULER"
           "RESOURCES"
           "NUMBER-OF-THREADS"
           "THREADS"

           "INVALID-RESOURCE-REQUIREMENT-ERROR"))
