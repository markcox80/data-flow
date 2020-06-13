(in-package "DATA-FLOW.COMPONENT")

(defclass basic-component (#+data-flow.features:threads
                           bt-mutex-component
                           #-data-flow.features:threads
                           sequential-component)
  ())

(defclass standard-component (basic-component)
  ())
