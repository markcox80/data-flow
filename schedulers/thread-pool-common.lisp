(in-package "DATA-FLOW.THREAD-POOL")

(defclass thread-pool ()
  ())

(defgeneric test-and-claim-resources (thread-pool runnable))
