(defsystem "data-flow-features"
  :author "Mark Cox"
  :description "Features framework for the data-flow system."
  :depends-on ("bordeaux-threads")
  :serial t
  :components ((:module "features"
                :serial t
                :components ((:file "packages")
                             (:file "features")))))
