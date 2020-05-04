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
  (let* ((scheduler (data-flow.resource-scheduler:make-resource-scheduler 5))
         (results (make-array 8 :initial-element '#:unset)))
    (dotimes (i (length results))
      (data-flow:schedule scheduler (let* ((index i))
                                      (lambda (scheduler)
                                        (setf (aref results index) (data-flow:blocking-allowed-p scheduler))))))
    (data-flow:execute scheduler)
    (is (eql (data-flow:number-of-threads scheduler)
             (count-if (lambda (value)
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
