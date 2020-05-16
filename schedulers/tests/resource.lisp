#-data-flow.features:threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:threads-unavailable-error))

(defpackage "DATA-FLOW.RESOURCE-SCHEDULER.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-RESOURCE-SCHEDULER-TESTS"))
(in-package "DATA-FLOW.RESOURCE-SCHEDULER.TESTS")
(5am:def-suite all-resource-scheduler-tests :in data-flow.scheduler.tests:all-scheduler-tests)
(5am:in-suite all-resource-scheduler-tests)

(test schedule/not-enough-resources
  (let* ((scheduler (data-flow.resource-scheduler:make-resource-scheduler 1 :resources 10)))
    (signals data-flow.resource-scheduler:invalid-resource-requirement-error
      (data-flow:schedule scheduler (constantly 1) :required-resources 15))
    (finishes (data-flow:schedule scheduler (constantly 2) :required-resources 10))
    (finishes (data-flow:schedule scheduler (constantly 3) :required-resources 0))))

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

  (let* ((scheduler (data-flow.resource-scheduler:make-resource-scheduler 5))
         (results (make-array 8 :initial-element '#:unset)))
    (dotimes (i (length results))
      (data-flow:schedule scheduler (let* ((index i))
                                      (lambda (scheduler)
                                        (setf (aref results index) (data-flow:blocking-allowed-p scheduler))))))
    (data-flow:execute scheduler)

    ;; This test is rather weak without barriers as it depends on a
    ;; number of external factors.
    (is-true (plusp (count-if (lambda (value)
                                (is-true (member value '(nil t)))
                                (eql value t))
                              results)))))

;;;; Add the resource scheduler to other test suites.

(defun make-parallel-scheduler (number-of-threads)
  (data-flow.resource-scheduler:make-resource-scheduler number-of-threads))

(defun make-scheduler ()
  (make-parallel-scheduler 1))

(pushnew 'make-scheduler data-flow.scheduler.tests:*scheduler-creation-functions*)
(pushnew 'make-parallel-scheduler data-flow.scheduler.parallel.tests:*scheduler-creation-functions*)
