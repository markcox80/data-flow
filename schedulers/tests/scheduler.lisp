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

(test schedule/remaining-runnables/execute1
  (do-schedulers (scheduler)
    (let* ((state1 nil)
           (state2 nil))
      (data-flow:schedule scheduler (let* ((executed? nil))
                                      (lambda ()
                                        (unless executed?
                                          (setf executed? t
                                                state1 (data-flow:schedule scheduler (constantly 1)))))))
      (data-flow:schedule scheduler (let* ((executed? nil))
                                      (lambda ()
                                        (unless executed?
                                          (setf executed? t
                                                state2 (data-flow:schedule scheduler (constantly 2)))))))
      (data-flow:execute1 scheduler)
      (is (= 2 (data-flow:count-remaining-runnables state1))
          "Incorrect number of remaining runnables for scheduler ~A. Expected 2 got ~d."
          scheduler
          (data-flow:count-remaining-runnables state1))
      (is (= 1 (data-flow:count-queued-runnables state1))
          "Incorrect number of queued runnables for scheduler ~A. Expected 1 got ~d."
          scheduler
          (data-flow:count-queued-runnables state1))
      (is (= 1 (data-flow:count-remaining-runnables state2))
          "Incorrect number of remaining runnables for scheduler ~A. Expected 1 got ~d."
          scheduler
          (data-flow:count-remaining-runnables state2))
      (is (= 2 (data-flow:count-queued-runnables state2))
          "Incorrect number of queued runnables for scheduler ~A. Expected 2 got ~d."
          (data-flow:count-queued-runnables state2)))))

(test schedule/remaining-runnables/execute
  (do-schedulers (scheduler)
    (let* ((state1 nil)
           (state2 nil))
      (data-flow:schedule scheduler (let* ((executed? nil))
                                      (lambda ()
                                        (unless executed?
                                          (setf executed? t
                                                state1 (data-flow:schedule scheduler (constantly 1)))))))
      (data-flow:schedule scheduler (let* ((executed? nil))
                                      (lambda ()
                                        (unless executed?
                                          (setf executed? t
                                                state2 (data-flow:schedule scheduler (constantly 2)))))))
      (data-flow:execute scheduler)
      (is (= 3 (data-flow:count-remaining-runnables state1))
          "Incorrect number of remaining runnables for scheduler ~A. Expected 2 got ~d."
          scheduler
          (data-flow:count-remaining-runnables state1))
      (is (= 0 (data-flow:count-queued-runnables state1))
          "Incorrect number of queued runnables for scheduler ~A. Expected 1 got ~d."
          scheduler
          (data-flow:count-queued-runnables state1))
      (is (= 3 (data-flow:count-remaining-runnables state2))
          "Incorrect number of remaining runnables for scheduler ~A. Expected 1 got ~d."
          scheduler
          (data-flow:count-remaining-runnables state2))
      (is (= 0 (data-flow:count-queued-runnables state2))
          "Incorrect number of queued runnables for scheduler ~A. Expected 2 got ~d."
          (data-flow:count-queued-runnables state2)))))

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
    (let* ((results (make-array 5 :initial-element 0))
           (runnable (lambda ()
                       (error 'test-error))))
      (data-flow:schedule scheduler runnable)
      (dotimes (i 5)
        (let* ((index i))
          (data-flow:schedule scheduler
                              (lambda ()
                                (incf (aref results index))
                                (data-flow:schedule scheduler
                                                    (lambda ()
                                                      (incf (aref results index))))))))
      (data-flow:start1 scheduler)
      (handler-case (progn
                      (data-flow:wait-until-finished scheduler)
                      (fail "No error condition was signalled."))
        (data-flow:execution-error (c)
          (is-true (typep (data-flow:execution-error-condition c)
                          'test-error))
          (is (eql (data-flow:execution-error-scheduler c)
                   scheduler))
          (is (eql (data-flow:execution-error-runnable c)
                   runnable))))
      (is-false (data-flow:executingp scheduler))
      (is-true (every #'(lambda (x) (= 1 x)) results))
      (data-flow:start1 scheduler)
      (data-flow:wait-until-finished scheduler)
      (is-true (every #'(lambda (x) (= 2 x)) results)))))

(test error-handling/reset
  (do-schedulers (scheduler)
    (let* ((runnable (lambda ()
                       (error 'test-error))))
      (data-flow:schedule scheduler runnable)
      (data-flow:start scheduler)
      (handler-case (progn
                      (data-flow:wait-until-finished scheduler)
                      (fail "No error condition was signalled."))
        (data-flow:execution-error (c)
          (is-true (typep (data-flow:execution-error-condition c)
                          'test-error))
          (is (eql (data-flow:execution-error-scheduler c)
                   scheduler))
          (is (eql (data-flow:execution-error-runnable c)
                   runnable))))
      (data-flow:start scheduler)
      (finishes (data-flow:wait-until-finished scheduler)))))

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

(test multiple-wait-until-finished-with-errors
  (do-schedulers (scheduler)
    (unwind-protect
         (dolist (start-fn (list #'data-flow:start1 #'data-flow:start))
           (let* ((runnable (lambda ()
                              (error 'division-by-zero))))
             (data-flow:schedule scheduler runnable)
             (funcall start-fn scheduler)
             (handler-case (data-flow:wait-until-finished scheduler)
               (data-flow:execution-error (c)
                 (is-true (typep (data-flow:execution-error-condition c) 'division-by-zero))
                 (is (eql runnable (data-flow:execution-error-runnable c)))
                 (is (eql scheduler (data-flow:execution-error-scheduler c))))
               (:no-error (&rest args)
                 (declare (ignore args))
                 (fail "Scheduler ~A did not signal an execution error." scheduler)))
             (finishes (data-flow:wait-until-finished scheduler))))
      (data-flow:cleanup scheduler))))

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

(test schedule-during-execution
  (do-schedulers (scheduler)
    (let* ((results (make-array 10 :initial-element nil)))
      (data-flow:schedule scheduler (lambda ()
                                      (dotimes (i (length results))
                                        (let* ((index i))
                                          (data-flow:schedule scheduler (lambda ()
                                                                          (setf (elt results index) t)))))))
      (data-flow:execute scheduler)
      (is-true (every #'(lambda (x)
                          (eql x t))
                      results)))))

(test blocking-allowed-p
  (do-schedulers (scheduler)
    (let* ((results (make-array 10 :initial-element '#:no-value)))
      (dotimes (i (length results))
        (data-flow:schedule scheduler (let* ((index i))
                                        (lambda ()
                                          (setf (aref results index) (data-flow:blocking-allowed-p scheduler))))))
      (data-flow:execute scheduler)
      (loop
        for i from 0 below (length results)
        for result = (elt results i)
        do
           (is-true (typep result '(or null (eql t))))

        count (not (null result)) into count-true

        finally
           (is (= 1 count-true))))))
