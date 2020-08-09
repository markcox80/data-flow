(in-package "DATA-FLOW.RESOURCE-SCHEDULER")


;;;; Resource protocol

(defgeneric test-resources-p (resource-pool resources))
(defgeneric test-and-claim-resources (resource-pool resources))
(defgeneric return-resources (resource-pool resources))

;;;; Resource protocol implementation for reals.

(defmethod test-resources-p ((remaining real) (required real))
  (check-type remaining (real 0))
  (check-type required (real 0))
  (<= required remaining))

(defmethod test-and-claim-resources ((remaining real) (required real))
  (check-type remaining (real 0))
  (check-type required (real 0))
  (when (<= required remaining)
    (- remaining required)))

(defmethod return-resources ((remaining real) (required real))
  (check-type remaining (real 0))
  (check-type required (real 0))
  (+ remaining required))


(defgeneric required-resources (runnable-with-resources))

(defclass runnable-with-resources ()
  ((resource-scheduler :initarg :resource-scheduler
                       :reader resource-scheduler)
   (%delegate :initarg :delegate
              :reader delegate-runnable)
   (%required-resources :initarg :required-resources
                        :reader required-resources)))

(defmethod data-flow.thread-pool:original-runnable ((runnable runnable-with-resources))
  (delegate-runnable runnable))

;;;; Resource Scheduler

(define-condition invalid-resource-requirement-error (error)
  ((%total-resources :initarg :total-resources
                     :reader invalid-resource-requirement-error-total-resources)
   (%required-resources :initarg :required-resources
                        :reader invalid-resource-requirement-error-required-resources)
   (%runnable :initarg :runnable
              :reader invalid-resource-requirement-error-runnable)
   (%scheduler :initarg :scheduler
               :reader invalid-resource-requirement-error-scheduler))
  (:report (lambda (condition stream)
             (format stream "The amount of resource required for ~A exceeds the total number of resources managed by the scheduler ~A i.e. ~A > ~A."
                     (invalid-resource-requirement-error-runnable condition)
                     (invalid-resource-requirement-error-scheduler condition)
                     (invalid-resource-requirement-error-required-resources condition)
                     (invalid-resource-requirement-error-total-resources condition)))))

(defclass resource-scheduler (data-flow.thread-pool:thread-pool)
  ((%resources :initarg :resources
               :reader resources)
   (%resources-function :initarg :resources-function
                        :initform (constantly 1)
                        :reader %resources-function)
   (%remaining-resources :initarg :remaining-resources
                         :accessor %remaining-resources)))

(defun make-resource-scheduler (number-of-threads
                                &key
                                  (resources number-of-threads)
                                  (resources-function (constantly 1))
                                  (poll-seconds data-flow.thread-pool:*default-poll-seconds*))
  (check-type number-of-threads (integer 1))
  (make-instance 'resource-scheduler
                 :resources resources
                 :remaining-resources resources
                 :number-of-threads number-of-threads
                 :resources-function resources-function
                 :poll-seconds poll-seconds))

(defmethod data-flow:run ((runnable runnable-with-resources))
  (unwind-protect (data-flow:run (delegate-runnable runnable))
    (let* ((resource-scheduler (resource-scheduler runnable))
           (required-resources (required-resources runnable)))
      (data-flow.sequential-object:linearize resource-scheduler
        (setf (%remaining-resources resource-scheduler) (return-resources (%remaining-resources resource-scheduler)
                                                                          required-resources))))))

(defmethod data-flow:schedule ((scheduler resource-scheduler) runnable &rest args &key required-resources &allow-other-keys)
  (alexandria:remove-from-plistf args :required-resources)
  (let* ((required-resources (or required-resources
                                 (funcall (%resources-function scheduler) runnable))))
    (unless (test-resources-p (resources scheduler) required-resources)
      (error 'invalid-resource-requirement-error
             :scheduler scheduler
             :runnable runnable
             :required-resources required-resources
             :total-resources (resources scheduler)))
    (apply #'call-next-method
           scheduler
           (make-instance 'runnable-with-resources
                          :resource-scheduler scheduler
                          :delegate runnable
                          :required-resources required-resources)
           args)))

(defmethod data-flow.thread-pool:execute-runnable-p ((scheduler resource-scheduler) (runnable runnable-with-resources))
  (let* ((required-resources (required-resources runnable))
         (remaining-resources (test-and-claim-resources (%remaining-resources scheduler) required-resources)))
    (when remaining-resources
      (setf (%remaining-resources scheduler) remaining-resources)
      t)))
