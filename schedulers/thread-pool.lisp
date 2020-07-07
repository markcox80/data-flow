(in-package "DATA-FLOW.THREAD-POOL")

;;;; Sequential thread pool

(defclass sequential-thread-pool (data-flow.sequential-scheduler:sequential-scheduler
                                  thread-pool)
  ())

(defmethod data-flow:number-of-threads ((scheduler sequential-thread-pool))
  (declare (ignore scheduler))
  1)


(defun make-thread-pool (number-of-threads &key (poll-seconds *default-poll-seconds*))
  (declare (ignorable poll-seconds))
  (check-type number-of-threads (integer 1))
  (check-type poll-seconds (real 0))
  (cond ((= 1 number-of-threads)
         (make-instance 'sequential-thread-pool))
        (t
         #+data-flow.features:threads
         (make-instance 'parallel-thread-pool
                        :number-of-threads number-of-threads
                        :poll-seconds *default-poll-seconds*)
         #-data-flow.features:threads
         (make-instance 'sequential-thread-pool))))
