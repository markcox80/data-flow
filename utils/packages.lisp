(defpackage "DATA-FLOW.UTILS"
  (:use "COMMON-LISP")

  #+data-flow.features:compare-and-set
  (:export "COMPARE-AND-SET"))
