(in-package "DATA-FLOW.CONTAINERS")

(defun make-srmw-fifo ()
  #+(and data-flow.features:threads data-flow.features:compare-and-set)
  (data-flow.lock-free-fifo:make-lock-free-fifo)
  #+(and data-flow.features:threads (not data-flow.features:compare-and-set))
  (data-flow.bt-mutex-queue:make-srmw-bt-mutex-queue (data-flow.fifo:make-fifo))
  #-data-flow.features:threads
  (data-flow.fifo:make-fifo))
