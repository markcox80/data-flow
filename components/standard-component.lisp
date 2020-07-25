(in-package "DATA-FLOW.COMPONENT")

(defclass standard-component (basic-component
                              data-flow.component.standard-port:standard-port-component-mixin)
  ())
