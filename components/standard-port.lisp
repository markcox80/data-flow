(in-package "DATA-FLOW.COMPONENT.STANDARD-PORT")

;;;; Events

(defclass port-event ()
  ((%port :initarg :port
          :reader port)))

(defclass value-written-event (port-event)
  ((%value :initarg :value
           :reader value)))

(defclass value-read-event (port-event)
  ())

(defclass port-disconnected-event (port-event)
  ())

;;;; Standard port

(defclass standard-port (data-flow:port)
  ((%component :initarg :component
               :reader %component)
   (%port-index :initarg :port-index
                :reader %port-index)
   (%remote-port :initarg :remote-port
                 :reader %remote-port)
   (%remote-component :initarg :remote-component
                      :reader %remote-component)
   (%connection :initarg :connection
                :accessor %connection)
   (%disconnectedp :initarg :disconnectedp
                   :initform nil
                   :accessor %disconnectedp)))

(defun %unseen-events-p (port)
  (check-type port standard-port)
  (ldb-test (byte 1 (%port-index port))
            (%unseen-events-integer (%component port))))

(defun (setf %unseen-events-p) (value port)
  (check-type value (member nil t))
  (check-type port standard-port)
  (setf (ldb (byte 1 (%port-index port))
             (%unseen-events-integer (%component port)))
        (if value
            1
            0))
  value)

(defmethod data-flow:process-all-events ((port standard-port))
  (data-flow:process-all-events (%component port))
  (setf (%unseen-events-p port) nil))

(defmethod data-flow:connection ((port standard-port))
  (data-flow:process-all-events port)
  (%connection port))

(defmethod data-flow:disconnect-port ((port standard-port))
  (data-flow:process-all-events port)
  (unless (%disconnectedp port)
    (setf (%disconnectedp port) t
          (%connection port) nil)
    (data-flow:enqueue-event (%remote-component port)
                             (make-instance 'port-disconnected-event :port (%remote-port port)))
    (data-flow.queue:enqueue (%disconnect-queue (%component port)) port)
    t))

;;;; Standard Input Port

(defclass standard-input-port (standard-port data-flow:input-port)
  ((%queue :initarg :queue
           :initform (data-flow.fifo:make-fifo)
           :reader %queue)))

(defmethod data-flow:read-value ((port standard-input-port) &key (errorp t) no-value-value disconnected-value)
  (data-flow:process-all-events port)

  (with-accessors ((%remote-component %remote-component)
                   (%remote-port %remote-port)
                   (%queue %queue))
      port
    (multiple-value-bind (item item?) (data-flow.queue:dequeue %queue)
      (cond (item?
             (unless (%disconnectedp port)
               (data-flow:enqueue-event %remote-component
                                        (make-instance 'value-read-event :port %remote-port)))
             item)

            ((%disconnectedp port)
             (if errorp
                 (error 'data-flow:port-disconnected-error :port port)
                 disconnected-value))

            (errorp
             (error 'data-flow:no-value-available-error :port port))

            (t
             no-value-value)))))

(defmethod data-flow:connection ((port standard-input-port))
  (data-flow:process-all-events port)
  (when (and (%disconnectedp port)
             (data-flow.queue:emptyp (%queue port)))
    (setf (%connection port) nil))
  (%connection port))

(defmethod data-flow:disconnect-port ((port standard-input-port))
  (when (call-next-method)
    (data-flow.queue:clear (%queue port))))

;;;; Standard Output Port

(defclass standard-output-port (standard-port data-flow:output-port)
  ((%total-space :initarg :total-space
                 :reader data-flow:total-space)
   (%available-space :initarg :available-space
                     :accessor %available-space)))

(defmethod data-flow:write-value (value (port standard-output-port) &key (errorp t) no-space-value disconnected-value &allow-other-keys)
  (data-flow:process-all-events port)

  (with-accessors ((%available-space %available-space)
                   (%remote-component %remote-component)
                   (%remote-port %remote-port))
      port
    (cond ((%disconnectedp port)
           (if errorp
               (error 'data-flow:port-disconnected-error :port port)
               disconnected-value))

          ((plusp %available-space)
           (decf %available-space)
           (data-flow:enqueue-event %remote-component
                                    (make-instance 'value-written-event
                                                   :port %remote-port
                                                   :value value))
           value)

          (errorp
           (error 'data-flow:no-space-available-error :port port))

          (t
           no-space-value))))

(defmethod data-flow:available-space ((port standard-output-port))
  (data-flow:process-all-events port)
  (%available-space port))

(defmethod data-flow:disconnect-port ((port standard-output-port))
  (when (call-next-method)
    (setf (%available-space port) 0)))

;;;; Component mixin

(defclass standard-port-component-mixin (data-flow.sequential-object:sequential-object)
  ((%unseen-events-integer :initform 0
                           :accessor %unseen-events-integer)
   (%next-port-index :initform 0
                     :accessor %next-port-index)
   (%disconnect-queue :initform (data-flow.fifo:make-fifo)
                      :reader %disconnect-queue)))

(defmethod data-flow:run :before ((component standard-port-component-mixin))
  ;; A component which does nothing shall not be executed again unless
  ;; an event is received during an invocation of run.
  (setf (%unseen-events-integer component) 0))

(defun process-disconnect-queue (disconnect-queue)
  (let* ((not-ready nil))
    (data-flow.queue:doqueue (port disconnect-queue)
      (cond ((typep port 'data-flow.component.disconnected-port:disconnected-port)
             ;; Port has already been disconnected.
             )

            ((data-flow:input-port-p port)
             (setf (%unseen-events-p port) nil)
             (cond ((data-flow.queue:emptyp (%queue port))
                    (change-class port 'data-flow.component.disconnected-port:disconnected-input-port))
                   (t
                    (push port not-ready))))

            ((data-flow:output-port-p port)
             (setf (%unseen-events-p port) nil)
             (change-class port 'data-flow.component.disconnected-port:disconnected-output-port
                           :total-space (data-flow:total-space port)))

            (t
             (error "The port ~A is neither an INPUT port nor an OUTPUT port." port))))
    (dolist (port not-ready)
      (data-flow.queue:enqueue disconnect-queue port)))
  (values))

(defmethod data-flow:run :after ((component standard-port-component-mixin))
  (process-disconnect-queue (%disconnect-queue component)))

(defmethod data-flow:process-event ((component standard-port-component-mixin) (event value-written-event))
  (let ((port (port event)))
    (check-type port standard-input-port)
    (unless (%disconnectedp port)
      (data-flow.queue:enqueue (%queue port) (value event))
      (setf (%unseen-events-p port) t)))
  (values))

(defmethod data-flow:process-event ((component standard-port-component-mixin) (event value-read-event))
  (let ((port (port event)))
    (check-type port standard-output-port)
    (unless (%disconnectedp port)
      (when (>= (%available-space port)
                (data-flow:total-space port))
        (warn "The component ~A received the event ~A for a value that was never sent."
              component event))
      (setf (%available-space port) (min (data-flow:total-space port)
                                         (1+ (%available-space port)))
            (%unseen-events-p port) t)))
  (values))

(defmethod data-flow:process-event ((component standard-port-component-mixin) (event port-disconnected-event))
  (let ((port (port event)))
    (check-type port standard-port)
    (unless (%disconnectedp port)
      (setf (%disconnectedp port) t
            (%unseen-events-p port) t)
      (data-flow.queue:enqueue (%disconnect-queue component) port)
      (etypecase port
        (standard-output-port
         (setf (%available-space port) 0
               (%connection port) nil))
        (standard-input-port
         (when (data-flow.queue:emptyp (%queue port))
           (setf (%connection port) nil))))))
  (values))

(defmethod data-flow:requires-execution-p or ((component standard-port-component-mixin))
  ;; There is no need to call process-all-events here as the
  ;; data-flow:component method checks to see if there are new events.
  ;;
  ;; This method checks to see if the component is yet to observe a
  ;; port state change caused by an invocation of PROCESS-ALL-EVENTS
  ;; during the invocation of RUN.
  (not (zerop (%unseen-events-integer component))))


;;;; Connection

(defclass standard-port-connection (data-flow:connection)
  ((%input-port :initarg :input-port
                :reader data-flow:input-port)
   (%input-component :initarg :input-component
                     :reader data-flow:input-component)
   (%output-port :initarg :output-port
                 :reader data-flow:output-port)
   (%output-component :initarg :output-component
                      :reader data-flow:output-component)))

(defmethod data-flow:connect-ports ((input-component data-flow.component.standard-port:standard-port-component-mixin)
                                    (input-port data-flow.component.disconnected-port:disconnected-input-port)
                                    (output-component data-flow.component.standard-port:standard-port-component-mixin)
                                    (output-port data-flow.component.disconnected-port:disconnected-output-port)
                                    &key total-space &allow-other-keys)
  ;; This method is concurrent with respect to the components and not the ports.
  (let* ((connection (make-instance 'standard-port-connection
                                    :input-port input-port
                                    :input-component input-component
                                    :output-port output-port
                                    :output-component output-component)))
    ;; Check that a call hasn't already succeeded.
    (unless (typep input-port 'data-flow.component.disconnected-port:disconnected-port)
      (error 'data-flow:already-connected-error :port input-port))

    (unless (typep output-port 'data-flow.component.disconnected-port:disconnected-port)
      (error 'data-flow:already-connected-error :port output-port))

    (data-flow.sequential-object:linearize input-component
      (let* ((port-index (%next-port-index input-component)))
        (incf (%next-port-index input-component))
        (change-class input-port 'standard-input-port
                      :component input-component
                      :port-index port-index
                      :remote-port output-port
                      :remote-component output-component
                      :connection connection
                      :disconnectedp nil)))

    (data-flow.sequential-object:linearize output-component
      (let* ((port-index (%next-port-index output-component))
             (total-space (or total-space
                              (data-flow:total-space output-port))))
        (incf (%next-port-index output-component))
        (change-class output-port 'standard-output-port
                      :component output-component
                      :port-index port-index
                      :remote-port input-port
                      :remote-component input-component
                      :connection connection
                      :disconnectedp nil
                      :total-space total-space
                      :available-space total-space)))

    connection))

(defmethod data-flow:connect-ports ((output-component data-flow.component.standard-port:standard-port-component-mixin)
                                    (output-port data-flow.component.disconnected-port:disconnected-output-port)
                                    (input-component data-flow.component.standard-port:standard-port-component-mixin)
                                    (input-port data-flow.component.disconnected-port:disconnected-input-port)
                                    &rest args &key &allow-other-keys)
  (apply #'data-flow:connect-ports
         input-component
         input-port
         output-component
         output-port
         args))

(defmethod data-flow:connect-ports ((component1 data-flow.component.standard-port:standard-port-component-mixin)
                                    (port1 data-flow.component.standard-port:standard-port)
                                    (component2 data-flow.component.standard-port:standard-port-component-mixin)
                                    (port2 data-flow:port)
                                    &key)
  (declare (ignore component1 component2 port2))
  (error 'data-flow:already-connected-error :port port1))

(defmethod data-flow:connect-ports ((component1 data-flow.component.standard-port:standard-port-component-mixin)
                                    (port1 data-flow:port)
                                    (component2 data-flow.component.standard-port:standard-port-component-mixin)
                                    (port2 data-flow.component.standard-port:standard-port)
                                    &key)
  (declare (ignore component1 port1 component2))
  (error 'data-flow:already-connected-error :port port2))
