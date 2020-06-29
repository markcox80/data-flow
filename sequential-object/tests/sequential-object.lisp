(in-package "DATA-FLOW.SEQUENTIAL-OBJECT.TESTS")
(in-suite all-sequential-object-tests)

(defun call-with-every-sequential-object (function)
  (dolist (object (list (make-instance 'data-flow.sequential-object:sequential-object)
                        (make-instance 'data-flow.sequential-object::single-threaded-sequential-object)
                        #+data-flow.features:threads
                        (make-instance 'data-flow.sequential-object::bt-sequential-object)
                        #+data-flow.features:compare-and-set
                        (make-instance 'data-flow.sequential-object::cas-sequential-object)))
    (funcall function object)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-sequential-objects ((var) &body body)
    `(call-with-every-sequential-object (lambda (,var)
                                          ,@body))))

(defun call-with-every-parallel-sequential-object (function)
  (do-sequential-objects (sequential-object)
    (when (typep sequential-object 'data-flow.sequential-object::parallel-sequential-object)
      (funcall function sequential-object))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-parallel-sequential-objects ((var) &body body)
    `(call-with-every-parallel-sequential-object (lambda (,var)
                                                   ,@body))))

(test basic-example
  (do-sequential-objects (object)
    (let* ((count 0))
      (dotimes (i 10)
        (data-flow.sequential-object:linearize object
          (incf count)))
      (is (= 10 count)))))

(test value-example
  (do-sequential-objects (object)
    (let* ((values (multiple-value-list (data-flow.sequential-object:linearize object
                                          (values 1 2 3 4)))))
      (is (equalp '(1 2 3 4) values)))))

(define-condition my-error (error)
  ())

(test error-example
  (do-sequential-objects (object)
    (signals my-error (data-flow.sequential-object:linearize object
                        (error 'my-error)))))

#+data-flow.features:threads
(test multithreaded-example
  (let* ((expected (loop
                     for index from 0 below 100000
                     sum (expt index 0.1))))
    (do-parallel-sequential-objects (object)
      (let* ((index 0)
             (value 0)
             (threads (loop
                        for thread-index from 0 below 10
                        collect
                        (bt:make-thread (lambda ()
                                          (sleep 0.25)
                                          (loop
                                            while (data-flow.sequential-object:linearize object
                                                    (when (< index 100000)
                                                      (incf value (expt index 0.1))
                                                      (incf index)
                                                      t))))))))
        (map nil #'bordeaux-threads:join-thread threads)
        (is (= expected value))))))
