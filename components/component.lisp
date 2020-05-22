(in-package "DATA-FLOW.COMPONENT")

(defclass component (#+data-flow.features:threads
                     bt-mutex-component
                     #-data-flow.features:threads
                     sequential-component)
  ())
