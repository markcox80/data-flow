
;;;; Component Tests
(defpackage "DATA-FLOW.COMPONENT.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-COMPONENT-TESTS"))
(in-package "DATA-FLOW.COMPONENT.TESTS")
(5am:def-suite all-component-tests :in data-flow.tests:all-data-flow-tests)

;;;; Disconnected Port Tests

(defpackage "DATA-FLOW.COMPONENT.DISCONNECTED-PORT.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-DISCONNECTED-PORT-TESTS"))
(in-package "DATA-FLOW.COMPONENT.DISCONNECTED-PORT.TESTS")
(5am:def-suite all-disconnected-port-tests :in data-flow.component.tests:all-component-tests)
