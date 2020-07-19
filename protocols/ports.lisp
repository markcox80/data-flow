(in-package "DATA-FLOW")

(defparameter +disconnected+ '#:disconnected)
(defparameter +no-space-available+ '#:no-space-available)
(defparameter +no-value-available+ '#:no-value-available)


;;;; Helpers

(defun %case-find-body (operator name body)
  (let ((value (find-if (lambda (item)
                          (cond ((listp item)
                                 (member name item))
                                ((symbolp item)
                                 (or (eql item t)
                                     (eql item name)))
                                (t
                                 (error "Invalid body expression for operator ~A." operator))))
                        body
                        :key #'first)))
    (when value
      `(progn ,@(rest value)))))

;;;; read-value-case

(defmacro read-value-case ((var port &rest args &key &allow-other-keys) &body body)
  (flet ((find-body (name)
           (%case-find-body 'read-value-case name body)))
    (alexandria:with-gensyms (port-var)
      `(let* ((,port-var ,port)
              (,var (read-value ,port-var
                                :disconnected-value +disconnected+
                                :no-value-value +no-value-available+
                                :errorp nil
                                ,@args)))
         (cond ((eql ,var +disconnected+)
                ,(or (find-body 'disconnected)
                     `(error 'port-disconnected-error :port ,port-var)))
               ((eql ,var +no-value-available+)
                ,(or (find-body 'no-value-available)
                     `(error 'no-value-available-error :port ,port-var)))
               (t
                ,(find-body 'success)))))))

(defmacro write-value-case ((value port &rest args &key &allow-other-keys) &body body)
  (flet ((find-body (name)
           (%case-find-body 'write-value-case name body)))
    (alexandria:with-gensyms (var port-var)
      `(let* ((,port-var ,port)
              (,var (data-flow:write-value ,value ,port-var
                                           :disconnected-value +disconnected+
                                           :no-space-value +no-space-available+
                                           :errorp nil
                                           ,@args)))
         (cond ((eql ,var +disconnected+)
                ,(or (find-body 'disconnected)
                     `(error 'port-disconnected-error :port ,port-var)))
               ((eql ,var +no-space-available+)
                ,(or (find-body 'no-space-available)
                     `(error 'no-space-available-error :port ,port-var)))
               (t
                ,(find-body 'success)))))))
