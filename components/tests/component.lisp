(in-package "DATA-FLOW.COMPONENT.TESTS")
(5am:in-suite all-component-tests)

(defclass test-component (data-flow.component:component)
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

#+data-flow.features:threads
(test mutex-component
  (let* ((scheduler (data-flow.resource-scheduler:make-resource-scheduler 2))
         (component (make-test-component scheduler
                                         (lambda (component)
                                           (events component)))))
    (data-flow:enqueue-event component 'hello)
    (data-flow:enqueue-event component 'there)
    (data-flow:execute scheduler)
    (is (equal '(hello there) (last-value component)))
    (data-flow:enqueue-event component 'world)
    (data-flow:execute scheduler)
    (is (equal '(world) (last-value component)))))
