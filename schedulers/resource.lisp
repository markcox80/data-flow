#-data-flow.features:threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:threads-unavailable-error))

(in-package "DATA-FLOW.RESOURCE-SCHEDULER")


(defgeneric required-resources (runnable-with-resources))

(defclass runnable-with-resources ()
  ((resource-scheduler :initarg :resource-scheduler
                       :reader resource-scheduler)
   (%delegate :initarg :delegate
              :reader delegate-runnable)
   (%required-resources :initarg :required-resources
                        :reader required-resources)))

;;;; Resource Scheduler

(deftype runnable-resources ()
  '(real 0))

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
  (check-type resources runnable-resources)
  (check-type number-of-threads (integer 1))
  (make-instance 'resource-scheduler
                 :resources resources
                 :remaining-resources resources
                 :number-of-threads number-of-threads
                 :resources-function resources-function
                 :poll-seconds poll-seconds))

(defmethod data-flow:run ((runnable runnable-with-resources))
  (unwind-protect (data-flow:run (delegate-runnable runnable))
    (data-flow.sequential-object:linearize (resource-scheduler runnable)
      (incf (%remaining-resources (resource-scheduler runnable))
            (required-resources runnable)))))

(defmethod data-flow:schedule ((scheduler resource-scheduler) runnable &key required-resources)
  (check-type required-resources (or null runnable-resources))
  (let* ((required-resources (or required-resources
                                (funcall (%resources-function scheduler) runnable))))
    (check-type required-resources runnable-resources)
    (when (> required-resources (resources scheduler))
      (error 'invalid-resource-requirement-error
             :scheduler scheduler
             :runnable runnable
             :required-resources required-resources
             :total-resources (resources scheduler)))
    (call-next-method scheduler (make-instance 'runnable-with-resources
                                               :resource-scheduler scheduler
                                               :delegate runnable
                                               :required-resources required-resources))))

(defmethod data-flow.thread-pool:test-and-claim-resources ((scheduler resource-scheduler) (runnable runnable-with-resources))
  (let* ((required-resources (required-resources runnable)))
    (when (<= required-resources
              (%remaining-resources scheduler))
      (decf (%remaining-resources scheduler)
            required-resources)
      (assert (not (minusp (%remaining-resources scheduler))))
      t)))
