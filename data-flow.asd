(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "data-flow-features"))

(defsystem "data-flow"
  :author "Mark Cox"
  :description "A framework for authoring and using components that process data arriving via ports."
  :depends-on ()
  :in-order-to ((test-op (test-op "data-flow/tests"))))

(defsystem "data-flow/protocols"
  :author "Mark Cox"
  :description "Protocols for the data-flow system."
  :serial t
  :components ((:module "protocols"
                :serial t
                :components ((:file "packages")
                             (:file "protocols")))))

(defsystem "data-flow/containers"
  :author "Mark Cox"
  :description "Containers for the data-flow system."
  :serial t
  :components ((:module "containers"
                :serial t
                :components ((:file "packages")
                             (:file "protocols")
                             (:file "fifo")
                             (:file "linked-list")
                             (:file "bt-mutex-queue" :if-feature data-flow.features:threads)))))

(defsystem "data-flow/schedulers"
  :author "Mark Cox"
  :description "Schedulers for the data-flow system."
  :serial t
  :depends-on ("data-flow/protocols" "data-flow/containers")
  :components ((:module "schedulers"
                :serial t
                :components ((:file "packages")
                             (:file "resource")))))

;;;; Tests

(defsystem "data-flow/tests/common"
  :author "Mark Cox"
  :description "Common functionality needed for the data-flow/tests system."
  :depends-on ("fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "asdf")))))

(defsystem "data-flow/containers/tests"
  :author "Mark Cox"
  :description "Tests for the data-flow/containers system."
  :depends-on ("data-flow/tests/common"
               "data-flow/containers")
  :serial t
  :components ((:module "containers/tests"
                :serial t
                :components ((:file "packages")
                             (:file "fifo")
                             (:file "linked-list")))))

(defsystem "data-flow/tests"
  :author "Mark Cox"
  :description "Tests for the data-flow system."
  :depends-on ("data-flow/tests/common"
               "data-flow/containers/tests"))
