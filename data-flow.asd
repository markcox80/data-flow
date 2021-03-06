(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "data-flow-features"))

(defsystem "data-flow/protocols"
  :author "Mark Cox"
  :description "Protocols for the data-flow system."
  :depends-on ("alexandria")
  :serial t
  :components ((:module "protocols"
                :serial t
                :components ((:file "packages")
                             (:file "protocols")
                             (:file "ports")))))

(defsystem "data-flow/utils"
  :author "Mark Cox"
  :description "Utilities for the data-flow system."
  :depends-on ("alexandria")
  :serial t
  :components ((:module "utils"
                :serial t
                :components ((:file "packages")
                             (:file "sbcl" :if-feature (:and :sbcl data-flow.features:compare-and-set))))))

(defsystem "data-flow/containers"
  :author "Mark Cox"
  :description "Containers for the data-flow system."
  :serial t
  :depends-on ("data-flow/utils")
  :components ((:module "containers"
                :serial t
                :components ((:file "packages")
                             (:file "protocols")
                             (:file "fifo")
                             (:file "linked-list")
                             (:file "bt-mutex-queue" :if-feature data-flow.features:threads)
                             (:file "lock-free-fifo" :if-feature data-flow.features:compare-and-set)
                             (:file "helpers")))))

(defsystem "data-flow/schedulers"
  :author "Mark Cox"
  :description "Schedulers for the data-flow system."
  :serial t
  :depends-on ("data-flow/protocols" "data-flow/containers" "data-flow/sequential-object")
  :components ((:module "schedulers"
                :serial t
                :components ((:file "packages")
                             (:file "common")
                             (:file "sequential")
                             (:file "thread-pool-common")
                             (:file "thread-pool-parallel" :if-feature data-flow.features:threads)
                             (:file "thread-pool")
                             (:file "resource")))))

(defsystem "data-flow/sequential-object"
  :author "Mark Cox"
  :description "Utilities for linearizing function calls."
  :serial t
  :depends-on ("data-flow/containers")
  :components ((:module "sequential-object"
                :serial t
                :components ((:file "packages")
                             (:file "protocols")
                             (:file "single-thread")
                             (:file "parallel")
                             (:file "bt" :if-feature data-flow.features:threads)
                             (:file "cas" :if-feature data-flow.features:compare-and-set)
                             (:file "sequential-object")))))

(defsystem "data-flow/components"
  :author "Mark Cox"
  :description "Component implementations for the data-flow system."
  :serial t
  :depends-on ("data-flow/protocols" "data-flow/containers" "data-flow/sequential-object")
  :components ((:module "components"
                :serial t
                :components ((:file "packages")
                             (:file "basic-component")
                             (:file "disconnected-port")
                             (:file "standard-port")
                             (:file "standard-component")))))

(defsystem "data-flow"
  :author "Mark Cox"
  :description "A framework for authoring and using components that process data arriving via ports."
  :depends-on ("data-flow/schedulers"
               "data-flow/components")
  :license "Simplified BSD License variant"
  :version "0.9.0"
  :in-order-to ((test-op (test-op "data-flow/tests"))))

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
                             (:file "linked-list")
                             (:file "lock-free-fifo" :if-feature data-flow.features:compare-and-set)))))

(defsystem "data-flow/schedulers/tests"
  :author "Mark Cox"
  :description "Tests for the data-flow/schedulers system."
  :depends-on ("data-flow/tests/common"
               "data-flow/schedulers")
  :serial t
  :components ((:module "schedulers/tests"
                :serial t
                :components ((:file "packages")
                             (:file "scheduler")
                             (:file "sequential")
                             (:file "parallel")
                             (:file "resource")
                             (:file "thread-pool")))))

(defsystem "data-flow/sequential-object/tests"
  :author "Mark Cox"
  :description "An abstraction for linearzing operations on an object."
  :depends-on ("data-flow/tests/common"
               "data-flow/sequential-object")
  :serial t
  :components ((:module "sequential-object/tests"
                :serial t
                :components ((:file "packages")
                             (:file "sequential-object")))))

(defsystem "data-flow/components/tests"
  :author "Mark Cox"
  :description "Tests for the data-flow/components system."
  :depends-on ("data-flow/tests/common"
               "data-flow/schedulers/tests"
               "data-flow/components")
  :serial t
  :components ((:module "components/tests"
                :serial t
                :components ((:file "packages")
                             (:file "component")
                             (:file "disconnected-port")
                             (:file "standard-port")))))

(defsystem "data-flow/tests"
  :author "Mark Cox"
  :description "Tests for the data-flow system."
  :depends-on ("data-flow/tests/common"
               "data-flow/containers/tests"
               "data-flow/schedulers/tests"
               "data-flow/sequential-object/tests"
               "data-flow/components/tests"))
