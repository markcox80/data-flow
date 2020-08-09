(in-package "DATA-FLOW.THREAD-POOL")

(defparameter *default-poll-seconds* 0.0001)

(defgeneric execute-runnable-p (thread-pool runnable))
(defgeneric original-runnable (runnable))

(defmethod original-runnable ((runnable t))
  runnable)
