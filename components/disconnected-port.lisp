(in-package "DATA-FLOW.COMPONENT.DISCONNECTED-PORT")

;;;; Disconnected Port

(defclass disconnected-port ()
  ())

(defmethod data-flow:connection ((port disconnected-port))
  (declare (ignore port))
  nil)

(defmethod data-flow:disconnect-port ((port disconnected-port))
  (declare (ignore port))
  (values))

;;;; Disconnected Input Port

(defclass disconnected-input-port (data-flow:input-port disconnected-port)
  ())

(defmethod data-flow:read-value ((port disconnected-input-port) &key (errorp t) no-value-value disconnected-value &allow-other-keys)
  (declare (ignore no-value-value))
  (if errorp
      (error 'data-flow:port-disconnected-error :port port)
      disconnected-value))

(defun data-flow:make-input-port ()
  (make-instance 'disconnected-input-port))

;;;; Disconnected Output Port

(defclass disconnected-output-port (data-flow:output-port disconnected-port)
  ((%total-space :initarg :total-space
                 :initform data-flow:*default-total-space*
                 :reader data-flow:total-space)))

(defmethod data-flow:write-value (value (port disconnected-output-port) &key (errorp t) no-space-value disconnected-value &allow-other-keys)
  (declare (ignore value no-space-value))
  (if errorp
      (error 'data-flow:port-disconnected-error :port port)
      disconnected-value))

(defmethod data-flow:space-available-p ((port disconnected-output-port))
  (declare (ignore port))
  nil)

(defmethod data-flow:available-space ((port disconnected-output-port))
  (declare (ignore port))
  0)

(defun data-flow:make-output-port (&key total-space)
  (check-type total-space (or null (integer 0)))
  (let* ((total-space (or total-space data-flow:*default-total-space*)))
    (check-type total-space (integer 0))
    (make-instance 'disconnected-output-port
                   :total-space total-space)))
