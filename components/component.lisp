(in-package "DATA-FLOW.COMPONENT")

(defclass component (#+data-flow.features:threads
                     mutex-component
                     #-data-flow.features:threads
                     sequential-component)
  ())
