
;;;; Test package for DATA-FLOW.CONTAINERS

(defpackage "DATA-FLOW.CONTAINERS.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "CONTAINER-TESTS"))
(in-package "DATA-FLOW.CONTAINERS.TESTS")
(5am:def-suite container-tests
  :in data-flow.tests:all-data-flow-tests)


;;;; Test package for DATA-FLOW.FIFO

(defpackage "DATA-FLOW.FIFO.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "FIFO-TESTS"))
(in-package "DATA-FLOW.FIFO.TESTS")
(5am:def-suite fifo-tests
  :in data-flow.containers.tests:container-tests)


;;;; Test package for DATA-FLOW.LINKED-LIST

(defpackage "DATA-FLOW.LINKED-LIST.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM"
        "DATA-FLOW.LINKED-LIST")
  (:export "LINKED-LIST-TESTS"))
(in-package "DATA-FLOW.LINKED-LIST.TESTS")
(5am:def-suite linked-list-tests
  :in data-flow.containers.tests:container-tests)
