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

(defpackage "DATA-FLOW.COMPONENT.STANDARD-PORT"
  (:use "COMMON-LISP")
  (:import-from "DATA-FLOW"
                "STANDARD-PORT-COMPONENT-MIXIN")
  (:export "STANDARD-PORT"
           "STANDARD-INPUT-PORT"
           "STANDARD-OUTPUT-PORT"

           "STANDARD-PORT-COMPONENT-MIXIN"
           "PORT-EVENT"
           "VALUE-WRITTEN-EVENT"
           "VALUE-READ-EVENT"
           "PORT-DISCONNECTED-EVENT"))
