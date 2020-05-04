(defpackage "DATA-FLOW.SCHEDULER.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-SCHEDULER-TESTS"
           "*SCHEDULER-CREATION-FUNCTIONS*"))
(in-package "DATA-FLOW.SCHEDULER.TESTS")
(fiveam:def-suite all-scheduler-tests :in data-flow.tests:all-data-flow-tests)
