#-data-flow.features:threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:threads-unavailable-error))

(defpackage "DATA-FLOW.SCHEDULER.PARALLEL.TESTS"
  (:use "COMMON-LISP"
        "FIVEAM")
  (:export "ALL-PARALLEL-SCHEDULER-TESTS"
           "*SCHEDULER-CREATION-FUNCTIONS*"))
(in-package "DATA-FLOW.SCHEDULER.PARALLEL.TESTS")
(5am:def-suite all-parallel-scheduler-tests :in data-flow.scheduler.tests:all-scheduler-tests)
(5am:in-suite all-parallel-scheduler-tests)

(defvar *scheduler-creation-functions* nil)

(defun call-with-every-parallel-scheduler (function number-of-threads)
  (loop
    for creation-function in *scheduler-creation-functions*
    for scheduler = (funcall creation-function number-of-threads)
    do
       (unwind-protect (funcall function scheduler)
         (data-flow:wait-until-finished scheduler)
         (data-flow:cleanup scheduler))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-parallel-schedulers ((var number-of-threads) &body body)
    `(call-with-every-parallel-scheduler (lambda (,var) ,@body) ,number-of-threads)))

(test parallel-scheduler-p
  (do-parallel-schedulers (scheduler 1)
    (is-true (typep scheduler 'data-flow:parallel-scheduler))))

(test start1/basic
  (do-parallel-schedulers (scheduler 5)
    (is (= 5 (data-flow:number-of-threads scheduler)))
    (data-flow:start1 scheduler)
    (let* ((threads (data-flow:threads scheduler)))
      (is (= 5 (length threads)))
      (unwind-protect (is (= 5 (length (intersection threads (bordeaux-threads:all-threads)))))
        (is-true (data-flow:wait-until-finished scheduler))
        (data-flow:cleanup scheduler))
      (is-true (every (complement #'bordeaux-threads:thread-alive-p) threads)))))

(test wait-until-finished
  (do-parallel-schedulers (scheduler 1)
    (let* ((lock (bordeaux-threads:make-lock "WAIT-UNTIL-FINISHED-LOCK"))
           (%state :started))
      (flet ((state ()
               (bordeaux-threads:with-lock-held (lock)
                 %state))
             ((setf state) (value)
               (bordeaux-threads:with-lock-held (lock)
                 (setf %state value))))
        (data-flow:schedule scheduler (lambda ()
                                        (setf (state) :running)
                                        (loop
                                          until (eql (state) :shutdown)
                                          do
                                             (sleep 0.2))
                                        (setf (state) :terminated)))
        (data-flow:start scheduler)
        (loop
          until (eql :running (state))
          do
             (is-false (data-flow:wait-until-finished scheduler :seconds 1)))
        (is-false (data-flow:wait-until-finished scheduler :seconds 1))
        (setf (state) :shutdown)
        (loop
          for count from 0 below 10
          until (data-flow:wait-until-finished scheduler :seconds 1))
        (is-true (data-flow:wait-until-finished scheduler :seconds 1))
        (is (eql :terminated (state)))))))
