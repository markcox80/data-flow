(in-package "DATA-FLOW.SEQUENTIAL-OBJECT")

(defclass sequential-object (#-data-flow.features:threads
                             single-threaded-sequential-object
                             #+data-flow.features:threads
                             bt-sequential-object)
  ())
