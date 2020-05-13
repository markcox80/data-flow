(defpackage "DATA-FLOW.SEQUENTIAL-SCHEDULER.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-SEQUENTIAL-SCHEDULER-TESTS"))
(in-package "DATA-FLOW.SEQUENTIAL-SCHEDULER.TESTS")
(5am:def-suite all-sequential-scheduler-tests :in data-flow.scheduler.tests:all-scheduler-tests)


;;;; Install the scheduler in existing tests.

(pushnew 'data-flow.sequential-scheduler:make-sequential-scheduler data-flow.scheduler.tests:*scheduler-creation-functions*)
