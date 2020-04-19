(defpackage "DATA-FLOW.QUEUE"
  (:use "COMMON-LISP")
  (:export "QUEUE"
           "ENQUEUE"
           "DEQUEUE"
           "DOQUEUE"))

(defpackage "DATA-FLOW.LINKED-LIST"
  (:use "COMMON-LISP")
  (:export "LINKED-LIST"
           "MAKE-LINKED-LIST"
           "EMPTYP"
           "PUSH-BACK"
           "POP-BACK"
           "PUSH-FRONT"
           "POP-FRONT"
           "FRONT"
           "BACK")

  (:export "LINK"
           "LINKP"
           "MAKE-LINK"
           "VALUE"
           "PREVIOUS"
           "NEXT"))

(defpackage "DATA-FLOW.FIFO"
  (:use "COMMON-LISP")
  (:export "FIFO"
           "MAKE-FIFO"))

(defpackage "DATA-FLOW.CONTAINERS"
  (:use "COMMON-LISP")
  (:import-from "DATA-FLOW.FIFO"
                "FIFO"
                "MAKE-FIFO")
  ;; Fifo
  (:export "FIFO"
           "MAKE-FIFO"))
