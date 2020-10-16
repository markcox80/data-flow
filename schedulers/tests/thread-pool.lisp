(in-package "DATA-FLOW.THREAD-POOL.TESTS")
(5am:in-suite all-thread-pool-tests)

(defun call-with-every-thread-pool (function &key (number-of-threads 1))
  (dolist (thread-pool (list #+data-flow.features:threads
                             (make-instance 'data-flow.thread-pool::parallel-thread-pool
                                            :number-of-threads number-of-threads)
                             (make-instance 'data-flow.thread-pool::sequential-thread-pool)
                             (data-flow.thread-pool:make-thread-pool number-of-threads)))
    (funcall function thread-pool)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-every-thread-pool ((var &key (number-of-threads 1)) &body body)
    `(call-with-every-thread-pool (lambda (,var)
                                    ,@body)
                                  :number-of-threads ,number-of-threads)))

(test blocking-allowed-p
  ;; The original intent of this test is to have concurrent tasks
  ;; check if blocking was allowed. Unfortunately, this is a bit
  ;; tricky without the use of barriers. Each task should 1) wait
  ;; until all threads are executing 2) test if blocking is allowed 3)
  ;; wait until all threads have determined if blocking is allowed 4)
  ;; carry on executing.
  ;;
  ;; Should the data-flow scheduler API provide support for barriers?
  ;; Barriers imply the existence of parallelism which is an
  ;; assumption I do not want in the data-flow system. Furthermore,
  ;; barriers assume a collection of threads are executing the same
  ;; code on different data. The data flow system is not meant for
  ;; this type of paralleism.

  (do-every-thread-pool (scheduler :number-of-threads 5)
    (let* ((results (make-array 8 :initial-element '#:unset)))
      (dotimes (i (length results))
        (data-flow:schedule scheduler (let* ((index i))
                                        (lambda ()
                                          (setf (aref results index) (data-flow:blocking-allowed-p scheduler))))))
      (data-flow:execute scheduler)

      ;; This test is rather weak without barriers as it depends on a
      ;; number of external factors.
      (is-true (plusp (count-if (lambda (value)
                                  (is-true (member value '(nil t)))
                                  (eql value t))
                                results))))))

(test make-thread-pool
  #+data-flow.features:threads
  (progn
    (is-true (typep (data-flow.thread-pool:make-thread-pool 2) 'data-flow:parallel-scheduler))
    (is-true (typep (data-flow.thread-pool:make-thread-pool 1) 'data-flow:sequential-scheduler)))

  #-data-flow.features:threads
  (let* ((thread-pool (data-flow.thread-pool:make-thread-pool 2)))
    (is-true (typep thread-pool 'data-flow:sequential-scheduler))
    (is (= 1 (data-flow:number-of-threads thread-pool))))

  (let* ((thread-pool (data-flow.thread-pool:make-thread-pool 1)))
    (is-true (typep thread-pool 'data-flow:sequential-scheduler))
    (is (= 1 (data-flow:number-of-threads thread-pool)))))

(test multiple-thread-pool-schedulers/execute1
  (let* ((scheduler1 (data-flow.thread-pool:make-thread-pool 1))
         (scheduler2 (data-flow.thread-pool:make-thread-pool 1))
         (task1 nil)
         (task2 nil)
         (task3 nil)
         (task4 nil))
    (data-flow:schedule scheduler1 (lambda ()
                                     (setf task1 t)
                                     (data-flow:schedule scheduler2 (lambda ()
                                                                      (setf task3 t)))))
    (data-flow:schedule scheduler2 (lambda ()
                                     (setf task2 t)
                                     (data-flow:schedule scheduler1 (lambda ()
                                                                      (setf task4 t)))))
    (data-flow:execute1 scheduler1 scheduler2)
    ;; Do not test TASK3 as it will be NIL if the call to START1 in
    ;; EXECUTE1 occurs before the first runnable above schedules the
    ;; task on SCHEDULER2. If START1 is called afterwards, then TASK3
    ;; will be T.
    (is-true (and task1 task2 (not task4)))

    (data-flow:execute1 scheduler1 scheduler2)
    (is-true (and task1 task2 task3 task4))))

(test multiple-thread-pool-schedulers/execute
  (let* ((scheduler1 (data-flow.thread-pool:make-thread-pool 1))
         (scheduler2 (data-flow.thread-pool:make-thread-pool 1))
         (task1 nil)
         (task2 nil)
         (task3 nil)
         (task4 nil))
    (data-flow:schedule scheduler1 (lambda ()
                                     (setf task1 t)
                                     (data-flow:schedule scheduler2 (lambda ()
                                                                      (setf task3 t)))))
    (data-flow:schedule scheduler2 (lambda ()
                                     (setf task2 t)
                                     (data-flow:schedule scheduler1 (lambda ()
                                                                      (setf task4 t)))))
    (data-flow:execute scheduler1 scheduler2)
    (is-true (and task1 task2 task3 task4))))

;;;; Add the resource scheduler to other test suites.

#+data-flow.features:threads
(progn
  (defun make-parallel-scheduler/parallel (number-of-threads)
    (make-instance 'data-flow.thread-pool::parallel-thread-pool
                   :number-of-threads number-of-threads))

  (defun make-scheduler/parallel ()
    (make-instance 'data-flow.thread-pool::parallel-thread-pool
                   :number-of-threads 1))

  (pushnew 'make-parallel-scheduler/parallel data-flow.scheduler.parallel.tests:*scheduler-creation-functions*)
  (pushnew 'make-scheduler/parallel data-flow.scheduler.tests:*scheduler-creation-functions*))

(defun make-scheduler/sequential ()
  (make-instance 'data-flow.thread-pool::sequential-thread-pool))

(pushnew 'make-scheduler/sequential data-flow.scheduler.tests:*scheduler-creation-functions*)
