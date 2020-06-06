(in-package "DATA-FLOW.SEQUENTIAL-OBJECT")

(defclass single-threaded-sequential-object ()
  ())

(defmethod linearized-apply ((function function) (object single-threaded-sequential-object))
  (funcall function))
