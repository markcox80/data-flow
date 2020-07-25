(in-package "DATA-FLOW.COMPONENT.TESTS")
(5am:in-suite all-component-tests)

(defun call-with-every-scheduler (function &key (number-of-threads 5))
  (check-type number-of-threads (integer 1))
  (data-flow.scheduler.tests::call-with-every-scheduler function)
  (data-flow.scheduler.parallel.tests::call-with-every-parallel-scheduler function number-of-threads))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-every-scheduler ((scheduler &key (number-of-threads 5)) &body body)
    `(call-with-every-scheduler (lambda (,scheduler)
                                  ,@body)
                                :number-of-threads ,number-of-threads)))

(defclass test-component (data-flow:basic-component)
  ((%delegate-function :initarg :delegate-function
                       :reader delegate-function)
   (%events :initarg :events
            :initform nil
            :accessor events)
   (%last-value :initarg :last-value
                :initform nil
                :accessor last-value)))

(defmethod reset ((component test-component))
  (setf (events component) nil
        (last-value component) nil))

(defmethod data-flow:process-event ((component test-component) event)
  (alexandria:appendf (events component) (list event)))

(defmethod data-flow:run ((component test-component))
  (setf (last-value component) (funcall (delegate-function component) component)
        (events component) nil))

(defun make-test-component (scheduler delegate-function)
  (make-instance 'test-component
                 :scheduler scheduler
                 :delegate-function delegate-function))

(test component-type
  (is-true (subtypep 'data-flow:basic-component 'data-flow:component))
  (is-true (subtypep 'data-flow:standard-component 'data-flow:basic-component)))

(test single-component
  (with-every-scheduler (scheduler)
    (let* ((component (make-test-component scheduler (lambda (component)
                                                       (events component)))))
      (data-flow:enqueue-event component 'hello)
      (data-flow:enqueue-event component 'there)
      (data-flow:execute scheduler)
      (is (equal '(hello there) (last-value component)))
      (data-flow:enqueue-event component 'world)
      (data-flow:execute scheduler)
      (is (equal '(world) (last-value component))))))

(test single-component/events-occurring-during-exection
  (flet ((make-test-lambda ()
           (let* ((invocation-count 0))
             (lambda (component)
               (when (zerop invocation-count)
                 (data-flow:enqueue-event component invocation-count))
               (incf invocation-count)
               (append (last-value component)
                       (events component))))))
    (with-every-scheduler (scheduler)
      (setf scheduler (data-flow.resource-scheduler:make-resource-scheduler 1))
      (let* ((component (make-test-component scheduler (make-test-lambda))))
        (data-flow:enqueue-event component 'hello)
        (data-flow:execute scheduler)
        (is (equalp '(hello 0) (last-value component)))
        (reset component)
        (is-true (null (last-value component)))
        (data-flow:execute scheduler)
        (is-true (null (last-value component)))))))

(test multiple-components
  (with-every-scheduler (scheduler)
    (let* ((sink (make-instance 'test-component
                                :scheduler scheduler
                                :delegate-function (lambda (component)
                                                     (append (last-value component)
                                                             (loop
                                                               for value in (events component)
                                                               collect (incf value))))))
           (source (make-instance 'test-component
                                  :scheduler scheduler
                                  :delegate-function (lambda (component)
                                                       (declare (ignore component))
                                                       (dotimes (i 10)
                                                         (data-flow:enqueue-event sink i))
                                                       t))))
      (data-flow:schedule scheduler source)
      (data-flow:execute scheduler)
      (is (equalp t (last-value source)))
      (is (equalp '(1 2 3 4 5 6 7 8 9 10) (last-value sink))))))

(test execution-state
  (with-every-scheduler (scheduler)
    (let ((component (make-test-component scheduler (lambda (component)
                                                      (data-flow:execution-state component)))))
      (is (eql :stopped (data-flow:execution-state component)))
      (data-flow:enqueue-event component 0)
      (is (eql :scheduled (data-flow:execution-state component)))
      (data-flow:execute scheduler)
      (is (eql :running (last-value component))))))

(test change-scheduler
  (let* ((scheduler1 (data-flow.sequential-scheduler:make-sequential-scheduler))
         (scheduler2 (data-flow.sequential-scheduler:make-sequential-scheduler))
         (condition nil)
         (component (make-test-component scheduler1 (lambda (component)
                                                      (setf condition
                                                            (nth-value 1
                                                                       (ignore-errors
                                                                        (setf (data-flow:scheduler component) scheduler1))))))))
    (is (eql scheduler1 (data-flow:scheduler component)))
    (setf (data-flow:scheduler component) scheduler2)
    (data-flow:enqueue-event component "hello")
    (signals simple-error (setf (data-flow:scheduler component) scheduler1))
    (is (eql scheduler2 (data-flow:scheduler component)))
    (data-flow:run (data-flow:make-component-lambda component))
    (is-true (typep condition 'simple-error))
    (setf (data-flow:scheduler component) scheduler1)
    (is (eql scheduler1 (data-flow:scheduler component)))))
