(in-package "DATA-FLOW.SCHEDULER")

(defgeneric original-runnable (runnable))

(defmethod original-runnable ((runnable t))
  runnable)

(defvar *error-output-sequential-object* (make-instance 'data-flow.sequential-object:sequential-object))

(defun run-with-error-handling/output-warning (scheduler condition &optional (stream *error-output*))
  (let* ((text (with-output-to-string (*standard-output*)
                 (pprint-logical-block (*standard-output* nil)
                   (format t "Ignoring unhandled error caught by scheduler ~A:" scheduler)
                   (pprint-newline :mandatory)
                   (pprint-newline :mandatory)
                   (pprint-logical-block (*standard-output* nil :per-line-prefix "  ")
                     (princ condition)
                     (pprint-newline :mandatory)
                     (pprint-newline :mandatory)
                     (format t "Error type: ~S" (type-of condition)))))))
    (data-flow.sequential-object:linearize *error-output-sequential-object*
      (write-string text stream))))

(defun run-with-error-handling (scheduler runnable &key (stream *error-output*))
  (let* ((error-condition nil))
    (restart-case (handler-bind ((error (lambda (condition)
                                          (setf error-condition condition)
                                          (ecase data-flow:*on-error*
                                            (:start1
                                             )
                                            (:debug
                                             (restart-case (invoke-debugger condition)
                                               (continue ())
                                               (start1 ())))
                                            (:ignore
                                             (invoke-restart 'ignore))
                                            (:warn-and-ignore
                                             (invoke-restart 'warn-and-ignore condition))
                                            (:warn-and-start1
                                             (run-with-error-handling/output-warning scheduler condition stream))))))
                    (data-flow:run runnable))
      (ignore ())
      (warn-and-ignore (&optional (condition error-condition))
        (run-with-error-handling/output-warning scheduler condition stream))))
  (values))
