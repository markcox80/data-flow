(defpackage "DATA-FLOW.COMPONENT.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-COMPONENT-TESTS"))
(in-package "DATA-FLOW.COMPONENT.TESTS")
(5am:def-suite all-component-tests :in data-flow.tests:all-data-flow-tests)
