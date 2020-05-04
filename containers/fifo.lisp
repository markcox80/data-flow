(in-package "DATA-FLOW.FIFO")

(defclass fifo (data-flow.queue:queue)
  ((%start
    :initarg :start
    :accessor start)
   (%end
    :initarg :end
    :accessor end)))

(defun make-fifo ()
  (let* ((cons (cons nil nil)))
    (make-instance 'fifo
                   :start cons
                   :end cons)))

(defmethod data-flow.queue:emptyp ((fifo fifo))
  (eql (start fifo)
       (end fifo)))

(defmethod data-flow.queue:enqueue ((fifo fifo) item)
  (let* ((new-end (cons nil nil))
         (current-end (end fifo)))
    (setf (car current-end) item
          (cdr current-end) new-end
          (end fifo) new-end))
  (values))

(defmethod data-flow.queue:dequeue ((fifo fifo))
  (cond ((eql (start fifo)
              (end fifo))
         (values nil
                 nil))
        (t
         (values (pop (start fifo))
                 t))))
