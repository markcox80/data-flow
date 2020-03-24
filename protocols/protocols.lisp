(in-package "DATA-FLOW")

;;;; Scheduler

(defgeneric run (scheduler runnable))

(defmethod run (scheduler (function function))
  (declare (ignore scheduler))
  (funcall function))

(defclass scheduler ()
  ())

(defgeneric schedule (scheduler runnable))
(defgeneric execute1 (scheduler))
(defgeneric execute-until (scheduler termination-function))
(defgeneric executingp (scheduler))
(defgeneric blocking-allowed-p (scheduler))

(define-condition already-executing-error (error)
  ((%scheduler :initarg :scheduler
               :reader already-executing-error-scheduler)))

(defclass sequential-scheduler (scheduler)
  ())

(defclass parallel-scheduler (scheduler)
  ())
