(in-package "DATA-FLOW.COMPONENT.DISCONNECTED-PORT.TESTS")
(5am:in-suite all-disconnected-port-tests)

(test disconnected-input-port
  (let* ((port (make-instance 'data-flow.component.disconnected-port:disconnected-input-port)))
    (is-true (data-flow:input-port-p port))
    (is-false (data-flow:output-port-p port))))

(test disconnected-output-port
  (let* ((port (make-instance 'data-flow.component.disconnected-port:disconnected-output-port)))
    (is-false (data-flow:input-port-p port))
    (is-true (data-flow:output-port-p port))

    (is-false (data-flow:space-available-p port))
    (is-true (zerop (data-flow:available-space port)))
    (is (= data-flow:*default-total-space* (data-flow:total-space port)))))

(test disconnected-port/closed
  (dolist (port (list (make-instance 'data-flow.component.disconnected-port:disconnected-input-port)
                      (make-instance 'data-flow.component.disconnected-port:disconnected-output-port)))
    (is-true (data-flow:portp port))
    (is-false (data-flow:connectedp port))
    (is-true (null (data-flow:connection port)))
    (is-true (typep port (type-of port)))))

(test disconnected-port/read-value
  (let* ((port (make-instance 'data-flow.component.disconnected-port:disconnected-input-port)))
    (signals data-flow:port-closed-error (data-flow:read-value port))
    (signals data-flow:port-closed-error (data-flow:read-value port :closed-value -1))
    (is (= -1 (data-flow:read-value port :closed-value -1 :errorp nil)))))

(test disconnected-port/write-value
  (let* ((port (make-instance 'data-flow.component.disconnected-port:disconnected-output-port)))
    (signals data-flow:port-closed-error (data-flow:write-value 1 port))
    (signals data-flow:port-closed-error (data-flow:write-value 1 port :closed-value -1))
    (is (= -1 (data-flow:write-value 1 port :closed-value -1 :errorp nil)))))
