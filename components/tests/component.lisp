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

(defclass test-component (data-flow:component)
  ((%delegate-function :initarg :delegate-function
                       :reader delegate-function)
   (%events :initarg :events
            :initform nil
            :accessor events)
   (%last-value :initarg :last-value
                :initform nil
                :accessor last-value)))

(defmethod data-flow:process-event ((component test-component) event)
  (alexandria:appendf (events component) (list event)))

(defmethod data-flow:run (scheduler (component test-component))
  (setf (last-value component) (funcall (delegate-function component) component)
        (events component) nil))

(defun make-test-component (scheduler function)
  (make-instance 'test-component
                 :scheduler scheduler
                 :delegate-function function))

(test component-example
  (with-every-scheduler (scheduler)
    (let* ((component (make-test-component scheduler
                                           (lambda (component)
                                             (events component)))))
      (data-flow:enqueue-event component 'hello)
      (data-flow:enqueue-event component 'there)
      (data-flow:execute scheduler)
      (is (equal '(hello there) (last-value component)))
      (data-flow:enqueue-event component 'world)
      (data-flow:execute scheduler)
      (is (equal '(world) (last-value component))))))
