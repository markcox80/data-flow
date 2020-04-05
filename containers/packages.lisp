(defpackage "DATA-FLOW.QUEUE"
  (:use "COMMON-LISP")
  (:export "QUEUE"
           "ENQUEUE"
           "DEQUEUE"
           "DOQUEUE"))

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
