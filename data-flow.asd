(defsystem "data-flow"
  :author "Mark Cox"
  :description "A framework for authoring and using components that process data arriving via ports."
  :depends-on ("data-flow/protocols"))

(defsystem "data-flow/protocols"
  :author "Mark Cox"
  :description "Protocols for the data-flow system."
  :serial t
  :components ((:module "protocols"
                :serial t
                :components ((:file "packages")
                             (:file "protocols")))))
