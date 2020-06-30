(in-package "DATA-FLOW.SEQUENTIAL-OBJECT")

(defclass sequential-object (#-data-flow.features:threads
                             single-threaded-sequential-object
                             #+(and data-flow.features:threads (not data-flow.features:compare-and-set))
                             bt-sequential-object
                             #+(and data-flow.features:threads data-flow.features:compare-and-set)
                             cas-sequential-object)
  ())
