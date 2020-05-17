(in-package "DATA-FLOW.SCHEDULER.TESTS")
(5am:in-suite all-scheduler-tests)

(defvar *scheduler-creation-functions* nil)

(defun call-with-every-scheduler (function)
  (loop
    for creation-function in *scheduler-creation-functions*
    for scheduler = (funcall creation-function)
    do
       (unwind-protect (funcall function scheduler)
         (data-flow:cleanup scheduler))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-schedulers ((var) &body body)
    `(call-with-every-scheduler (lambda (,var)
                                  ,@body))))

(test schedulerp
  (do-schedulers (scheduler)
    (is-true (typep scheduler 'data-flow:scheduler))))

(test scheduler-state-p
  (do-schedulers (scheduler)
    (let* ((state (data-flow:schedule scheduler (constantly 1))))
      (is-true (typep state 'data-flow:scheduler-state)))))

(test schedule/queued-runnables
  (do-schedulers (scheduler)
    (let* ((state1 (data-flow:schedule scheduler (constantly 1)))
           (state2 (data-flow:schedule scheduler (constantly 2))))
      (is (= 1 (data-flow:count-queued-runnables state1)))
      (is (= 2 (data-flow:count-queued-runnables state2)))
      (data-flow:start scheduler)
      (data-flow:wait-until-finished scheduler)
      (let* ((state3 (data-flow:schedule scheduler (constantly 3))))
        (is (= 1 (data-flow:count-queued-runnables state3)))))))

(test schedule/remaining-runnables
  (do-schedulers (scheduler)
    (let* ((count1 -1)
           (count2 -1))
      (data-flow:schedule scheduler (let* ((executed? nil))
                                      (lambda ()
                                        (unless executed?
                                          (setf executed? t)
                                          (let* ((state (data-flow:schedule scheduler (constantly 1))))
                                            (setf count1 (data-flow:count-remaining-runnables state)))))))
      (data-flow:schedule scheduler (let* ((executed? nil))
                                      (lambda ()
                                        (unless executed?
                                          (setf executed? t)
                                          (let* ((state (data-flow:schedule scheduler (constantly 2))))
                                            (setf count2 (data-flow:count-remaining-runnables state)))))))
      (data-flow:execute1 scheduler)
      (when (> count1 count2)
        (psetf count1 count2
               count2 count1))
      (is (= 1 count1))
      (is (= 2 count2)))))

(test executingp
  (do-schedulers (scheduler)
    (is-false (data-flow:executingp scheduler))
    (let* ((result '#:unset))
      (data-flow:schedule scheduler (lambda ()
                                      (setf result (data-flow:executingp scheduler))))
      (data-flow:execute scheduler)
      (is-false (data-flow:executingp scheduler))
      (is (eql t result)))))

(define-condition test-error (error)
  ())

(test error-handling/simple
  (do-schedulers (scheduler)
    (let* ((results (make-array 5 :initial-element 0)))
      (data-flow:schedule scheduler (lambda ()
                                      (error 'test-error)))
      (dotimes (i 5)
        (let* ((index i))
          (data-flow:schedule scheduler
                              (lambda ()
                                (incf (aref results index))
                                (data-flow:schedule scheduler
                                                    (lambda ()
                                                      (incf (aref results index))))))))
      (data-flow:start1 scheduler)
      (signals test-error (data-flow:wait-until-finished scheduler))
      (is-false (data-flow:executingp scheduler))
      (is-true (every #'(lambda (x) (= 1 x)) results))
      (data-flow:start1 scheduler)
      (data-flow:wait-until-finished scheduler)
      (is-true (every #'(lambda (x) (= 2 x)) results)))))

(test error-handling/reset
  (do-schedulers (scheduler)
    (data-flow:schedule scheduler (lambda ()
                                    (error 'test-error)))
    (data-flow:start scheduler)
    (signals test-error (data-flow:wait-until-finished scheduler))
    (data-flow:start scheduler)
    (finishes (data-flow:wait-until-finished scheduler))))

(test multiple-starts
  (do-schedulers (scheduler)
    (dolist (start-fn (list #'data-flow:start1 #'data-flow:start))
      (data-flow:schedule scheduler (lambda ()
                                      (sleep 1)))
      (funcall start-fn scheduler)
      (finishes (funcall start-fn scheduler))
      (data-flow:wait-until-finished scheduler)
      (data-flow:cleanup scheduler)
      (finishes (data-flow:cleanup scheduler)))))

(test cleanup-sans-wait-until-finished
  (do-schedulers (scheduler)
    (let* ((results (make-array 5 :initial-element 0)))
      (flet ((make-function (index)
               (lambda ()
                 (incf (aref results index)))))

        (dotimes (i 5)
          (data-flow:schedule scheduler (make-function i))))

      (data-flow:start scheduler)
      (data-flow:cleanup scheduler)
      (is-false (data-flow:executingp scheduler))
      (is-true (data-flow:wait-until-finished scheduler))
      (is-true (every #'(lambda (x) (eql x 1)) results)))))
