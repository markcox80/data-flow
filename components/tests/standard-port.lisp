(in-package "DATA-FLOW.COMPONENT.STANDARD-PORT.TESTS")
(5am:in-suite all-standard-port-tests)

(defclass test-event ()
  ())

(defclass test-component (data-flow:basic-component
                          data-flow.component.standard-port:standard-port-component-mixin)
  ((%function :initarg :function
              :initform nil
              :accessor test-component-function)))

(defmethod data-flow:run ((component test-component))
  (let* ((f (test-component-function component)))
    (when f
      (funcall f))))

(defmethod data-flow:process-event ((component test-component) (event test-event))
  (declare (ignore component event)))

(test types
  (is-true (subtypep 'data-flow.component.standard-port:standard-port
                     'data-flow:port))
  (is-true (subtypep 'data-flow.component.standard-port:standard-input-port
                     'data-flow:input-port))
  (is-true (subtypep 'data-flow.component.standard-port:standard-output-port
                     'data-flow:output-port)))

(test connection
  (let* ((src (make-instance 'test-component))
         (src-port (data-flow:make-output-port))
         (sink (make-instance 'test-component))
         (sink-port (data-flow:make-input-port))
         (connection (data-flow:connect-ports src src-port sink sink-port)))
    (is-true (data-flow:connectedp src-port))
    (is-true (data-flow:connectedp sink-port))
    (is-true (typep connection 'data-flow:connection))

    (is (eql sink (data-flow:input-component connection)))
    (is (eql sink-port (data-flow:input-port connection)))

    (is (eql src (data-flow:output-component connection)))
    (is (eql src-port (data-flow:output-port connection)))

    (is (eql connection (data-flow:connection src-port)))
    (is (eql connection (data-flow:connection sink-port)))))

(test connection/already-connected-error
  (let* ((src (make-instance 'test-component))
         (src-port (data-flow:make-output-port))
         (sink (make-instance 'test-component))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)
    (is-true (data-flow:connectedp src-port))
    (is-true (data-flow:connectedp sink-port))

    (handler-case (progn
                    (data-flow:connect-ports src src-port sink (data-flow:make-input-port))
                    (fail "Test failed to signal ~A." 'data-flow:already-connected-error))
      (data-flow:already-connected-error (e)
        (is (eql src-port (data-flow:port-error-port e)))))

    (is-true (typep src-port 'data-flow.component.standard-port:standard-output-port))
    (is-true (typep sink-port 'data-flow.component.standard-port:standard-input-port))

    (handler-case (progn
                    (data-flow:connect-ports src (data-flow:make-output-port) sink sink-port)
                    (fail "Test failed to signal ~A." 'data-flow:already-connected-error))
      (data-flow:already-connected-error (e)
        (is (eql sink-port (data-flow:port-error-port e)))))

    (is-true (typep src-port 'data-flow.component.standard-port:standard-output-port))
    (is-true (typep sink-port 'data-flow.component.standard-port:standard-input-port))))

(test connection/two-input-ports
  (let* ((c1 (make-instance 'test-component))
         (p1 (data-flow:make-input-port))
         (p2 (data-flow:make-input-port)))
    (is-true (null (compute-applicable-methods #'data-flow:connect-ports
                                               (list c1 p1 c1 p2))))))

(test connection/two-output-ports
  (let* ((c1 (make-instance 'test-component))
         (p1 (data-flow:make-output-port))
         (p2 (data-flow:make-output-port)))
    (is-true (null (compute-applicable-methods #'data-flow:connect-ports
                                               (list c1 p1 c1 p2))))))

(test methods-call-process-all-events
  (labels ((perform-test-helper (test-name port-type function)
             (check-type port-type (member :input :output))
             (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
                    (src (make-instance 'test-component :scheduler scheduler))
                    (src-port (data-flow:make-output-port))
                    (sink (make-instance 'test-component :scheduler scheduler))
                    (sink-port (data-flow:make-input-port)))
               (data-flow:connect-ports src src-port sink sink-port)
               (is-false (data-flow:requires-execution-p src))
               (is-false (data-flow:requires-execution-p sink))

               (ecase port-type
                 (:input
                  (data-flow:enqueue-event sink (make-instance 'test-event))
                  (is-true (data-flow:requires-execution-p sink))
                  (funcall function sink-port)
                  (is-false (data-flow:requires-execution-p sink)))
                 (:output
                  (data-flow:enqueue-event src (make-instance 'test-event))
                  (is-true (data-flow:requires-execution-p src))
                  (funcall function src-port)
                  (is-false (data-flow:requires-execution-p src)
                            "The subtest of methods-call-process-all-events with name ~A with port type ~A does not process all events."
                            test-name port-type)))))
           (perform-test (test-name port-type function)
             (case port-type
               (:both
                (perform-test-helper test-name :input function)
                (perform-test-helper test-name :output function))
               (t
                (perform-test-helper test-name port-type function)))))
    (macrolet ((do-test ((test-name var port-type) &body body)
                 `(perform-test ',test-name ,port-type (lambda (,var)
                                                         ,@body))))
      (do-test (disconnect-port port :both)
        (data-flow:disconnect-port port))

      (do-test (connection port :both)
        (is (typep (data-flow:connection port) 'data-flow:connection)))

      (do-test (connectedp port :both)
        (is-true (data-flow:connectedp port)))

      (do-test (read-value port :input)
        (is-true (null (data-flow:read-value port :errorp nil))))

      (do-test (write-value port :output)
        (data-flow:write-value 1 port))

      (do-test (space-available-p port :output)
        (is-true (data-flow:space-available-p port)))

      (do-test (available-space port :output)
        (is (= data-flow:*default-total-space* (data-flow:available-space port)))))))

(test methods-see-port-events
  (labels ((perform-test-helper (test-name port-type function)
             (check-type port-type (member :input :output))
             (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
                    (src (make-instance 'test-component :scheduler scheduler))
                    (src-port1 (data-flow:make-output-port))
                    (src-port2 (data-flow:make-output-port))
                    (sink (make-instance 'test-component :scheduler scheduler))
                    (sink-port1 (data-flow:make-input-port))
                    (sink-port2 (data-flow:make-input-port)))
               (data-flow:connect-ports src src-port1 sink sink-port1)
               (data-flow:connect-ports src src-port2 sink sink-port2)
               (ecase port-type
                 (:output
                  (data-flow:disconnect-port sink-port1)
                  (data-flow:disconnect-port sink-port2)
                  (is-true (data-flow:requires-execution-p src))
                  (funcall function src-port1)
                  (is-true (data-flow:requires-execution-p src))
                  (data-flow:disconnect-port src-port2)
                  (is-false (data-flow:requires-execution-p src)
                            "The subtest ~A does not 'see' the events on one of the ports." test-name)
                  (is-false (data-flow:requires-execution-p sink)))
                 (:input
                  (data-flow:disconnect-port src-port1)
                  (data-flow:disconnect-port src-port2)
                  (is-true (data-flow:requires-execution-p sink))
                  (funcall function sink-port1)
                  (is-true (data-flow:requires-execution-p sink))
                  (data-flow:disconnect-port sink-port2)
                  (is-false (data-flow:requires-execution-p sink)
                            "The subtest ~A does not 'see' the events on one of the ports." test-name)
                  (is-false (data-flow:requires-execution-p src))))))
           (perform-test (test-name port-type function)
             (ecase port-type
               (:both
                (perform-test-helper test-name :input function)
                (perform-test-helper test-name :output function))
               ((:input :output)
                (perform-test-helper test-name port-type function)))))
    (macrolet ((do-test ((test-name port-var port-type) &body body)
                 `(perform-test ',test-name ',port-type (lambda (,port-var) ,@body))))
      (do-test (disconnect-port port :both)
        (data-flow:disconnect-port port))

      (do-test (connection port :both)
        (is-true (null (data-flow:connection port))))

      (do-test (connectedp port :both)
        (is-false (data-flow:connectedp port)))

      (do-test (read-value port :input)
        (is-true (null (data-flow:read-value port :errorp nil))))

      (do-test (write-value port :output)
        (is-true (null (data-flow:write-value 1 port :errorp nil))))

      (do-test (space-available-p port :output)
        (is-false (data-flow:space-available-p port)))

      (do-test (available-space port :output)
        (is-true (zerop (data-flow:available-space port)))))))

(test disconnect-port
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (sink (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)

    (is-true (typep sink-port 'data-flow.component.standard-port:standard-input-port))
    (is-true (data-flow:connectedp sink-port))

    ;; Disconnect the sink port
    (data-flow:disconnect-port sink-port)
    (is-false (data-flow:connectedp sink-port))
    (is-true (data-flow:requires-execution-p src))
    (is-false (data-flow:requires-execution-p sink))
    (is-true (typep sink-port 'data-flow.component.standard-port:standard-input-port))
    (is-true (typep src-port 'data-flow.component.standard-port:standard-output-port))

    ;; Check that the event is propagated to the src component.
    (data-flow:process-all-events src) ; This is not required as connectedp should already call it.
                                        ; I have put this here for my sake.
    (is-false (data-flow:connectedp src-port))
    (is-true (typep src-port 'data-flow.component.standard-port:standard-output-port))

    ;; Disconnect the src-port and ensure no event is propagated to
    ;; the sink.
    (data-flow:disconnect-port src-port)
    (is-false (data-flow:requires-execution-p sink))

    ;; Run the components to ensure the ports change to a disconnected port.
    (data-flow:run sink)
    (is-true (typep sink-port 'data-flow.component.disconnected-port:disconnected-input-port))
    (is-true (typep src-port 'data-flow.component.standard-port:standard-output-port))

    (data-flow:run src)
    (is-true (typep sink-port 'data-flow.component.disconnected-port:disconnected-input-port))
    (is-true (typep src-port 'data-flow.component.disconnected-port:disconnected-output-port))))

(test port-disconnected-event
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)
    (data-flow:enqueue-event sink (make-instance 'data-flow.component.standard-port:port-disconnected-event
                                                 :port sink-port))
    (data-flow:disconnect-port sink-port)
    ;; No event should be sent to src because DISCONNECT-PORT must
    ;; process any events first. The sent port-disconnected-event
    ;; should disconnect the port before the body of DISCONNECT-PORT
    ;; can act on the port.
    (is-false (data-flow:requires-execution-p src))))

(test disconnect-port-changes-class
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)
    (data-flow:disconnect-port sink-port)
    (data-flow:run src)
    (data-flow:run sink)
    (is-true (typep src-port 'data-flow.component.disconnected-port:disconnected-port))
    (is-true (typep sink-port 'data-flow.component.disconnected-port:disconnected-port))))

(test disconnect-output-port
  ;; Ensure the total space is preserved.
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 5))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)
    (data-flow:disconnect-port src-port)
    (data-flow:run src)
    (is-true (typep src-port 'data-flow.component.disconnected-port:disconnected-output-port))
    (is (= 5 (data-flow:total-space src-port)))))

(test write-read
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port1 (data-flow:make-output-port :total-space 1))
         (src-port2 (data-flow:make-output-port :total-space 1))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port1 (data-flow:make-input-port))
         (sink-port2 (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port1 sink sink-port1)
    (data-flow:connect-ports src src-port2 sink sink-port2)

    (data-flow:write-value 1 src-port1)
    (data-flow:write-value 2 src-port2)

    (is-true (data-flow:requires-execution-p sink))
    (is (= 1 (data-flow:read-value sink-port1)))
    (is-true (data-flow:requires-execution-p sink))
    (is (= 2 (data-flow:read-value sink-port2)))
    (is-false (data-flow:requires-execution-p sink))

    (is-true (data-flow:requires-execution-p src))

    (data-flow:write-value 3 src-port1)
    (is-true (data-flow:requires-execution-p src))
    (data-flow:write-value 4 src-port2)
    (is-false (data-flow:requires-execution-p src))

    (is-true (data-flow:requires-execution-p sink))
    (is (= 3 (data-flow:read-value sink-port1)))))

(test available-capacity
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 2))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)
    (is (= 2 (data-flow:total-space src-port)))
    (is (= 2 (data-flow:available-space src-port)))
    (is-true (data-flow:space-available-p src-port))

    (data-flow:write-value 1 src-port)
    (is (= 2 (data-flow:total-space src-port)))
    (is (= 1 (data-flow:available-space src-port)))
    (is-true (data-flow:space-available-p src-port))
    (data-flow:write-value 2 src-port)
    (is (= 2 (data-flow:total-space src-port)))
    (is (= 0 (data-flow:available-space src-port)))
    (is-false (data-flow:space-available-p src-port))

    (is (= 1 (data-flow:read-value sink-port)))
    (is (= 2 (data-flow:total-space src-port)))
    (is (= 1 (data-flow:available-space src-port)))
    (is-true (data-flow:available-space src-port))

    (is (= 2 (data-flow:read-value sink-port)))
    (is (= 2 (data-flow:total-space src-port)))
    (is (= 2 (data-flow:available-space src-port)))
    (is-true (data-flow:space-available-p src-port))

    (data-flow:write-value 3 src-port)
    (is (= 1 (data-flow:available-space src-port)))
    (is-true (data-flow:space-available-p src-port))

    (is (= 3 (data-flow:read-value sink-port)))
    (is (= 2 (data-flow:available-space src-port)))
    (is-true (data-flow:space-available-p src-port))))

(test multiple-disconnect-port-events
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port1 (data-flow:make-output-port))
         (src-port2 (data-flow:make-output-port))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port1 (data-flow:make-input-port))
         (sink-port2 (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port1 sink sink-port1)
    (data-flow:connect-ports src src-port2 sink sink-port2)

    (data-flow:write-value 1 src-port1)
    (data-flow:disconnect-port src-port2)

    (is-true (data-flow:requires-execution-p sink))
    (is (= 1 (data-flow:read-value sink-port1)))
    (is-true (data-flow:requires-execution-p sink))
    (is (null (data-flow:read-value sink-port2 :errorp nil)))
    (is-false (data-flow:requires-execution-p sink))

    (is-true (data-flow:space-available-p src-port1))
    (is-false (data-flow:requires-execution-p src))

    (data-flow:disconnect-port src-port2)
    (is-false (data-flow:requires-execution-p sink))

    (data-flow:disconnect-port sink-port1)
    (is-true (data-flow:requires-execution-p src))

    (is-false (data-flow:connectedp src-port1))
    (is-false (data-flow:requires-execution-p src))
    (is-false (data-flow:connectedp src-port2))

    (is-false (data-flow:connectedp sink-port1))
    (is-false (data-flow:connectedp sink-port2))))

(test read-disconnected-port
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 5))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)
    (data-flow:write-value 1 src-port)
    (data-flow:write-value 2 src-port)
    (data-flow:disconnect-port src-port)

    (is-false (data-flow:connectedp src-port))
    (is-true (data-flow:connectedp sink-port))

    (is (= 1 (data-flow:read-value sink-port :errorp nil)))
    (is (= 2 (data-flow:read-value sink-port :errorp nil)))
    (is-false (data-flow:connectedp sink-port))))

(test read-value
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)
    (data-flow:write-value 1 src-port)
    (is (= 1 (data-flow:read-value sink-port)))

    (signals data-flow:no-value-available-error
      (data-flow:read-value sink-port))

    (signals data-flow:no-value-available-error
      (data-flow:read-value sink-port :no-value-value nil))

    (is-true (= -1 (data-flow:read-value sink-port :no-value-value -1 :errorp nil)))

    (data-flow:disconnect-port src-port)

    (signals data-flow:port-disconnected-error
      (data-flow:read-value sink-port))

    (signals data-flow:port-disconnected-error
      (data-flow:read-value sink-port :disconnected-value nil))

    (is-true (= -2 (data-flow:read-value sink-port :disconnected-value -2 :errorp nil)))))

(test write-value
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 2))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)

    (flet ((test-no-space-available (port)
             (is-false (data-flow:space-available-p port))
             (signals data-flow:no-space-available-error
               (data-flow:write-value 100 port))
             (signals data-flow:no-space-available-error
               (data-flow:write-value 100 port :no-space-value -1))
             (is (= -1 (data-flow:write-value 100 port :no-space-value -1 :errorp nil)))))

      (is (= 2 (data-flow:available-space src-port)))
      (is-true (data-flow:space-available-p src-port))
      (is (eql 1 (data-flow:write-value 1 src-port)))

      (is (= 1 (data-flow:available-space src-port)))
      (is-true (data-flow:space-available-p src-port))
      (is (eql 2 (data-flow:write-value 2 src-port)))

      (test-no-space-available src-port)

      (is (= 1 (data-flow:read-value sink-port)))
      (is (eql 3 (data-flow:write-value 3 src-port)))

      (test-no-space-available src-port)

      (data-flow:disconnect-port src-port)

      (is-false (data-flow:space-available-p src-port))
      (is-true (zerop (data-flow:available-space src-port)))
      (signals data-flow:port-disconnected-error
        (data-flow:write-value 4 src-port))

      (signals data-flow:port-disconnected-error
        (data-flow:write-value 4 src-port :disconnected-value nil))

      (is (= -1 (data-flow:write-value 4 src-port :disconnected-value -1 :errorp nil))))))

(test read-value/values-after-disconnection
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 2))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)
    (data-flow:write-value 1 src-port)
    (data-flow:write-value 2 src-port)
    (data-flow:disconnect-port src-port)

    (is-false (data-flow:requires-execution-p src))

    ;; The sink port has data so this should be true.
    (is-true (data-flow:connectedp sink-port))

    (is (eql 1 (data-flow:read-value sink-port)))
    ;; No events should be sent to src because the port is disconnected.
    (is-false (data-flow:requires-execution-p src))

    (is (eql 2 (data-flow:read-value sink-port)))
    (is-false (data-flow:connectedp sink-port))
    (is-true (null (data-flow:read-value sink-port :errorp nil :disconnected-value nil)))

    (is-false (data-flow:requires-execution-p src))))

(test read-value/values-after-disconnection/disconnected-port
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port src sink-port :total-space 5)
    (dotimes (i 5)
      (data-flow:write-value i src-port))
    (data-flow:run src)
    (is (= 0 (data-flow:read-value sink-port)))

    (data-flow:run src)
    (data-flow:disconnect-port src-port)
    (is (= 1 (data-flow:read-value sink-port)))

    (data-flow:run src)
    (is (= 2 (data-flow:read-value sink-port)))
    (is (= 3 (data-flow:read-value sink-port)))

    (data-flow:run src)
    (is (= 4 (data-flow:read-value sink-port)))
    (signals data-flow:port-disconnected-error (data-flow:read-value sink-port))
    (is-true (typep sink-port 'data-flow.component.standard-port:standard-port))

    (data-flow:run src)
    (signals data-flow:port-disconnected-error (data-flow:read-value sink-port))
    (is-true (typep sink-port 'data-flow.component.disconnected-port:disconnected-port))))

(test read-and-write-events
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 2))
         (sink (make-instance 'test-component :scheduler scheduler))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port sink sink-port)

    (is-false (data-flow:requires-execution-p src))
    (is-false (data-flow:requires-execution-p sink))

    (data-flow:write-value 1 src-port)

    (is-false (data-flow:requires-execution-p src))
    (is-true (data-flow:requires-execution-p sink))

    (data-flow:write-value 2 src-port)

    (is-false (data-flow:requires-execution-p src))
    (is-true (data-flow:requires-execution-p sink))

    (is (eql 1 (data-flow:read-value sink-port)))

    (is-true (data-flow:requires-execution-p src))
    (is-false (data-flow:requires-execution-p sink))

    (data-flow:write-value 3 src-port)

    (is-false (data-flow:requires-execution-p src))
    (is-true (data-flow:requires-execution-p sink))))

(test read-value-case/success
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port src sink-port)

    (data-flow:write-value 1 src-port)
    (let* ((success? nil))
      (data-flow:read-value-case (value sink-port)
        (data-flow:success
         (setf success? t)))
      (is-true success?))

    (data-flow:write-value 2 src-port)
    (let* ((success? t))
      (data-flow:read-value-case (value sink-port)
        (data-flow:no-value-available
         (setf success? nil))
        (data-flow:disconnected
         (setf success? nil)))
      (is-true success?))

    (data-flow:write-value 3 src-port)
    (let* ((success? t))
      (data-flow:read-value-case (value sink-port)
        ((data-flow:no-value-available data-flow:disconnected)
         (setf success? nil)))
      (is-true success?))

    (data-flow:write-value 4 src-port)
    (finishes (data-flow:read-value-case (value sink-port)))))

(test read-value-case/no-value-available
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port src sink-port)

    (signals data-flow:no-value-available-error (data-flow:read-value-case (value sink-port)))

    (let* ((no-value-available? nil))
      (data-flow:read-value-case (value sink-port)
        (data-flow:no-value-available
         (setf no-value-available? t)))
      (is-true no-value-available?))

    (let* ((no-value-available? nil))
      (data-flow:read-value-case (value sink-port)
        ((data-flow:no-value-available)
         (setf no-value-available? t)))
      (is-true no-value-available?))))

(test read-value-case/disconnected
  (let* ((sink-port (data-flow:make-input-port)))
    (signals data-flow:port-disconnected-error (data-flow:read-value-case (value sink-port)))

    (let* ((disconnected? nil))
      (data-flow:read-value-case (value sink-port)
        (data-flow:disconnected
         (setf disconnected? t)))
      (is-true disconnected?))

    (let* ((disconnected? nil))
      (data-flow:read-value-case (value sink-port)
        ((data-flow:disconnected)
         (setf disconnected? t)))
      (is-true disconnected?))))

(test write-value-case/success
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 1))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port src sink-port)

    (let* ((success? nil))
      (data-flow:write-value-case (1 src-port)
        (data-flow:success
         (setf success? t)))
      (is-true success?))
    (is (eql 1 (data-flow:read-value sink-port)))

    (let* ((success? nil))
      (data-flow:write-value-case (2 src-port)
        ((data-flow:success)
         (setf success? t)))
      (is-true success?))
    (is (eql 2 (data-flow:read-value sink-port)))

    (finishes (data-flow:write-value-case (3 src-port)))
    (is (eql 3 (data-flow:read-value sink-port)))))

(test write-value-case/no-space-available
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (src (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 1))
         (sink-port (data-flow:make-input-port)))
    (data-flow:connect-ports src src-port src sink-port)

    (data-flow:write-value 1 src-port)

    (signals data-flow:no-space-available-error (data-flow:write-value-case (2 src-port)))

    (let* ((no-space-available? nil))
      (data-flow:write-value-case (3 src-port)
        (data-flow:no-space-available
         (setf no-space-available? t)))
      (is-true no-space-available?))

    (let* ((no-space-available? nil))
      (data-flow:write-value-case (3 src-port)
        ((data-flow:no-space-available)
         (setf no-space-available? t)))
      (is-true no-space-available?))))

(test write-value-case/disconnected
  (let* ((src-port (data-flow:make-output-port :total-space 1)))

    (signals data-flow:port-disconnected-error (data-flow:write-value-case (1 src-port)))

    (let* ((disconnected? nil))
      (data-flow:write-value-case (3 src-port)
        (data-flow:disconnected
         (setf disconnected? t)))
      (is-true disconnected?))

    (let* ((disconnected? nil))
      (data-flow:write-value-case (3 src-port)
        ((data-flow:disconnected)
         (setf disconnected? t)))
      (is-true disconnected?))))

(test connect-ports/total-space
  (let* ((scheduler (data-flow.sequential-scheduler:make-sequential-scheduler))
         (component (make-instance 'test-component :scheduler scheduler))
         (src-port (data-flow:make-output-port :total-space 1))
         (sink-port (data-flow:make-input-port)))
    (loop
      for total-space from 1 to 10
      do
         (data-flow:connect-ports component src-port component sink-port :total-space total-space)
         (is (= total-space (data-flow:total-space src-port)))
         (is (= total-space (data-flow:available-space src-port)))
         (data-flow:disconnect-port src-port)
         (data-flow:disconnect-port sink-port)
         (data-flow:run component))
    (is (= 10 (data-flow:total-space src-port)))))
