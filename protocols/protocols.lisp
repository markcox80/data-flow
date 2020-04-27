(in-package "DATA-FLOW")

;;;; Scheduler

(defgeneric run (scheduler runnable))

(defmethod run (scheduler (function function))
  (funcall function scheduler))

(defclass scheduler ()
  ())

(defgeneric schedule (scheduler runnable))
(defgeneric blocking-allowed-p (scheduler))
(defgeneric executingp (scheduler))
(defgeneric start (scheduler))
(defgeneric wait-until-finished (scheduler))
(defgeneric cleanup (scheduler))
(defgeneric execute (scheduler))

(define-condition already-executing-error (error)
  ((%scheduler :initarg :scheduler
               :reader already-executing-error-scheduler)))

(defclass sequential-scheduler (scheduler)
  ())

(defclass parallel-scheduler (scheduler)
  ())
