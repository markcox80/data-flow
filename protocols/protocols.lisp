(in-package "DATA-FLOW")

;;;; Scheduler

(defgeneric run (scheduler runnable))

(defmethod run (scheduler (function function))
  (funcall function scheduler))

(defclass scheduler ()
  ())

(defgeneric schedule (scheduler runnable &key &allow-other-keys))
(defgeneric blocking-allowed-p (scheduler))
(defgeneric executingp (scheduler))
(defgeneric start1 (scheduler))
(defgeneric start (scheduler))
(defgeneric wait-until-finished (scheduler &key seconds &allow-other-keys))
(defgeneric cleanup (scheduler))
(defgeneric execute (scheduler))

(define-condition already-executing-error (error)
  ((%scheduler :initarg :scheduler
               :reader already-executing-error-scheduler)))

(defclass sequential-scheduler (scheduler)
  ())

(defclass parallel-scheduler (scheduler)
  ())
