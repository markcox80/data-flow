(defpackage "DATA-FLOW.SCHEDULER.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-SCHEDULER-TESTS"
           "*SCHEDULER-CREATION-FUNCTIONS*"))
(in-package "DATA-FLOW.SCHEDULER.TESTS")
(fiveam:def-suite all-scheduler-tests :in data-flow.tests:all-data-flow-tests)

(defpackage "DATA-FLOW.THREAD-POOL.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-THREAD-POOL-TESTS"))
(in-package "DATA-FLOW.THREAD-POOL.TESTS")
(fiveam:def-suite all-thread-pool-tests :in data-flow.scheduler.tests:all-scheduler-tests)
