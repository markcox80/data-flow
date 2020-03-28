(defpackage "DATA-FLOW.CONTAINERS.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "CONTAINER-TESTS"))
(in-package "DATA-FLOW.CONTAINERS.TESTS")
(5am:def-suite container-tests
  :in data-flow.tests:all-data-flow-tests)

(defpackage "DATA-FLOW.FIFO.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "FIFO-TESTS"))
(in-package "DATA-FLOW.FIFO.TESTS")
(5am:def-suite fifo-tests
  :in data-flow.containers.tests:container-tests)
