(defpackage "DATA-FLOW.COMPONENT"
  (:use "COMMON-LISP")
  (:import-from "DATA-FLOW"
                "BASIC-COMPONENT"
                "STANDARD-COMPONENT")
  (:export "BASIC-COMPONENT"
           "STANDARD-COMPONENT"))

(defpackage "DATA-FLOW.COMPONENT.DISCONNECTED-PORT"
  (:use "COMMON-LISP")
  (:export "DISCONNECTED-PORT"
           "DISCONNECTED-INPUT-PORT"
           "DISCONNECTED-OUTPUT-PORT"))
