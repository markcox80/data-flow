#-data-flow.features:compare-and-set
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error 'data-flow.features:compare-and-set-unavailable-error))

(in-package "DATA-FLOW.LOCK-FREE-FIFO")

(defclass lock-free-fifo (data-flow.queue:queue)
  ((%head :initarg :head)
   (%tail :initarg :tail)))

(defun make-lock-free-fifo ()
  (let* ((sentinel (cons nil nil)))
    (make-instance 'lock-free-fifo
                   :head sentinel
                   :tail sentinel)))

(defun head-and-tail (fifo)
  (check-type fifo lock-free-fifo)
  (with-slots (%head %tail) fifo
    (loop
      for current-head = %head
      for current-tail = %tail
      do
         (when (and (eq current-head %head)
                    (eq current-tail %tail))
           (if (cdr current-tail)
               (data-flow.utils:compare-and-set %tail current-tail (cdr current-tail))
               (return (values current-head current-tail)))))))

(defmethod data-flow.queue:clear ((fifo lock-free-fifo))
  (with-slots (%head) fifo
    (loop
      until (multiple-value-bind (head tail) (head-and-tail fifo)
              (data-flow.utils:compare-and-set %head head tail))))
  (values))

(defmethod data-flow.queue:emptyp ((fifo lock-free-fifo))
  (multiple-value-bind (head tail) (head-and-tail fifo)
    (eq head tail)))

(defmethod data-flow.queue:enqueue ((fifo lock-free-fifo) item)
  (with-slots (%tail) fifo
    (loop
      with new-cons = (cons item nil)
      do
         (multiple-value-bind (head tail) (head-and-tail fifo)
           (declare (ignore head))
           (when (data-flow.utils:compare-and-set (cdr tail) nil new-cons)
             (data-flow.utils:compare-and-set %tail tail new-cons)
             (return)))))
  (values))

(defmethod data-flow.queue:dequeue ((fifo lock-free-fifo))
  (with-slots (%head) fifo
    (loop
      (multiple-value-bind (head tail) (head-and-tail fifo)
        (cond ((eq head tail)
               (return (values nil nil)))
              ((data-flow.utils:compare-and-set %head head (cdr head))
               (return (values (car (cdr head)) t))))))))
