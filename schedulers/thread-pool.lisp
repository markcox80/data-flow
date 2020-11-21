(in-package "DATA-FLOW.THREAD-POOL")

;;;; Sequential thread pool

(defclass sequential-thread-pool (data-flow.sequential-scheduler:sequential-scheduler
                                  data-flow.sequential-object:sequential-object)
  ())

(defmethod initialize-instance :after ((instance sequential-thread-pool) &key number-of-threads poll-seconds)
  (declare (ignore instance number-of-threads poll-seconds)))

(defmethod data-flow:number-of-threads ((scheduler sequential-thread-pool))
  (declare (ignore scheduler))
  1)

;;;; Thread pool class.

(defclass thread-pool (#+data-flow.features:threads
                       parallel-thread-pool
                       #-data-flow.features:threads
                       sequential-thread-pool)
  ())

(defclass thread-pool/one-thread (thread-pool data-flow:sequential-scheduler)
  ())


(defun make-thread-pool (number-of-threads &key (poll-seconds *default-poll-seconds*))
  (declare (ignorable number-of-threads poll-seconds))
  (check-type number-of-threads (integer 1))
  (check-type poll-seconds (real 0))
  #+data-flow.features:threads
  (make-instance (cond ((= 1 number-of-threads)
                        'thread-pool/one-thread)
                       (t
                        'thread-pool))
                 :number-of-threads number-of-threads
                 :poll-seconds poll-seconds)
  #-data-flow.features:threads
  (make-instance 'sequential-thread-pool))
