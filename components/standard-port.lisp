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

(defclass port-closed-event (port-event)
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
   (%closedp :initarg :closedp
             :initform nil
             :accessor %closedp)))

(defun %unseen-events-p (port)
  (check-type port standard-port)
  (ldb-test (byte 1 (%port-index port))
            (%unseen-events-integer (%component port))))

(defun (setf %unseen-events-p) (value port)
  (check-type port standard-port)
  (setf (ldb (byte 1 (%port-index port))
             (%unseen-events-integer (%component port)))
        (if value
            1
            0))
  value)

(defmethod data-flow:port-closed-p ((port standard-port))
  (data-flow:process-all-events (%component port))
  (%closedp port))

(defmethod data-flow:connection ((port standard-port))
  (data-flow:process-all-events (%component port))
  (%connection port))

(defmethod data-flow:close-port ((port standard-port))
  (data-flow:process-all-events (%component port))
  (unless (%closedp port)
    (setf (%closedp port) t
          (%unseen-events-p port) nil)
    (data-flow:enqueue-event (%remote-component port)
                             (make-instance 'port-closed-event :port (%remote-port port)))
    t))

(defmethod data-flow:disconnect-port ((port standard-port))
  (data-flow:process-all-events (%component port))
  (unless (%closedp port)
    (setf (%closedp port) t
          (%unseen-events-p port) nil)
    (data-flow:enqueue-event (%remote-component port)
                             (make-instance 'port-disconnected-event :port (%remote-port port))))
  (values))

;;;; Standard Input Port

(defclass standard-input-port (standard-port data-flow:input-port)
  ((%queue :initarg :queue
           :initform (data-flow.fifo:make-fifo)
           :reader %queue)))

(defmethod data-flow:read-value ((port standard-input-port) &key (errorp t) no-data-value closed-value)
  (data-flow:process-all-events port)
  (setf (%unseen-events-p port) nil)

  (with-accessors ((%remote-component %remote-component)
                   (%remote-port %remote-port)
                   (%queue %queue))
      port
    (multiple-value-bind (item item?) (data-flow.queue:dequeue %queue)
      (cond (item?
             (data-flow:enqueue-event %remote-component
                                      (make-instance 'value-read-event :port %remote-port))
             item)

            ((%closedp port)
             (if errorp
                 (error 'data-flow:port-closed-error :port port)
                 closed-value))

            (errorp
             (error 'data-flow:no-data-available-error :port port))

            (t
             no-data-value)))))

(defmethod data-flow:port-closed-p ((port standard-input-port))
  (data-flow:process-all-events port)
  (and (data-flow.queue:emptyp (%queue port))
       (%closedp port)))

(defmethod data-flow:close-port ((port standard-input-port))
  (when (call-next-method)
    (data-flow.queue:clear (%queue port))))

(defmethod data-flow:disconnect-port ((port standard-input-port))
  (call-next-method)
  (change-class port 'data-flow.component.disconnected-port:disconnected-input-port))

;;;; Standard Output Port

(defclass standard-output-port (standard-port data-flow:output-port)
  ((%total-space :initarg :total-space
                 :reader data-flow:total-space)
   (%available-space :initarg :available-space
                     :accessor %available-space)))

(defmethod data-flow:write-value (value (port standard-output-port) &key (errorp t) no-space-value closed-value &allow-other-keys)
  (data-flow:process-all-events port)
  (setf (%unseen-events-p port) nil)

  (with-accessors ((%available-space %available-space)
                   (%remote-component %remote-component)
                   (%remote-port %remote-port))
      port
    (cond ((%closedp port)
           (if errorp
               (error 'data-flow:port-closed-error :port port)
               closed-value))

          ((plusp %available-space)
           (decf %available-space)
           (data-flow:enqueue-event %remote-component
                                    (make-instance 'value-written-event
                                                   :port %remote-port
                                                   :value value)))

          (errorp
           (error 'data-flow:no-space-available-error :port port))

          (t
           no-space-value))))

(defmethod data-flow:available-space ((port standard-output-port))
  (data-flow:process-all-events port)
  (setf (%unseen-events-p port) nil)
  (%available-space port))

(defmethod data-flow:close-port ((port standard-output-port))
  (when (call-next-method)
    (setf (%available-space port) 0)))

(defmethod data-flow:disconnect-port ((port standard-output-port))
  (call-next-method)
  (change-class port 'data-flow.component.disconnected-port:disconnected-output-port
                :total-space (data-flow:total-space port)))

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
  (data-flow.queue:doqueue (port disconnect-queue)
    (cond ((typep port 'data-flow.component.disconnected-port:disconnected-port)
           ;; Port has already been disconnected.
           )

          ((data-flow:input-port-p port)
           (setf (%unseen-events-p port) nil)
           (change-class port 'data-flow.component.disconnected-port:disconnected-input-port))

          ((data-flow:output-port-p port)
           (setf (%unseen-events-p port) nil)
           (change-class port 'data-flow.component.disconnected-port:disconnected-output-port
                         :total-space (data-flow:total-space port)))

          (t
           (error "The port ~A is neither an INPUT port nor an OUTPUT port." port)))))

(defmethod data-flow:run :after ((component standard-port-component-mixin))
  (process-disconnect-queue (%disconnect-queue component)))

(defmethod data-flow:process-event ((component standard-port-component-mixin) (event value-written-event))
  (let ((port (port event)))
    (check-type port standard-input-port)
    (unless (%closedp port)
      (data-flow.queue:enqueue (%queue port) (value event))
      (setf (%unseen-events-p port) t)))
  (values))

(defmethod data-flow:process-event ((component standard-port-component-mixin) (event value-read-event))
  (let ((port (port event)))
    (check-type port standard-output-port)
    (unless (%closedp port)
      (when (>= (%available-space port)
                (data-flow:total-space port))
        (warn "The component ~A received the event ~A for a value that was never sent."
              component event))
      (setf (%available-space port) (min (data-flow:total-space port)
                                         (1+ (%available-space port)))
            (%unseen-events-p port) t)))
  (values))

(defmethod data-flow:process-event ((component standard-port-component-mixin) (event port-closed-event))
  (declare (ignore component))
  (let ((port (port event)))
    (check-type port standard-port)
    (unless (%closedp port)
      (setf (%closedp port) t
            (%unseen-events-p port) t)
      (when (typep port 'standard-output-port)
        (setf (%available-space port) 0))))
  (values))

(defmethod data-flow:process-event ((component standard-port-component-mixin) (event port-disconnected-event))
  ;; It would be tempting to invoke CHANGE-CLASS here but you need to
  ;; consider what happens after PROCESS-ALL-EVENTS is called. Have a
  ;; look at READ-VALUE and you can see what needs to change in order
  ;; to handle a port changing class. That same logic needs to appear
  ;; in all operations which call PROCESS-ALL-EVENTS.
  ;;
  ;; Instead, we close the port and then make a note to disconnect the
  ;; port after the component has run.
  ;;
  ;; I am not sure a design involving CHANGE-CLASS is a good idea. It
  ;; does save checking that all of the slots have been initialised
  ;; correctly.
  (let ((port (port event)))
    (check-type port standard-port)
    (unless (%closedp port)
      (setf (%closedp port) t
            (%unseen-events-p port) t)
      (when (typep port 'standard-output-port)
        (setf (%available-space port) 0))
      (data-flow.queue:enqueue port (%disconnect-queue component))))
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
                                    &key &allow-other-keys)
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
                      :closedp nil)))

    (data-flow.sequential-object:linearize output-component
      (let* ((port-index (%next-port-index output-component)))
        (incf (%next-port-index output-component))
        (change-class output-port 'standard-output-port
                      :component output-component
                      :port-index port-index
                      :remote-port input-port
                      :remote-component input-component
                      :connection connection
                      :closedp nil
                      :total-space (data-flow:total-space output-port)
                      :available-space (data-flow:total-space output-port))))

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
