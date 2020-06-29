(in-package "DATA-FLOW.LOCK-FREE-FIFO.TESTS")
(in-suite all-lock-free-fifo-tests)

(defun make-enqueuer (fifo start end)
  (lambda ()
    (loop
      for x from start below end
      do
         (data-flow.queue:enqueue x fifo))
    (values)))

(defun make-dequeuer (fifo start end)
  (assert (< start end))
  (let* ((good t)
         (count 0)
         (min-value end)
         (max-value (1- start)))
    (lambda (&optional good-value-p)
      (cond (good-value-p
             (and good
                  (= count (- end start))
                  (= min-value start)
                  (= max-value (1- end))))
            (t
             (loop
               with end-time = (+ (get-universal-time) 30)
               while (and (< (get-universal-time) end-time)
                          (< count (- end start)))
               do
                  (multiple-value-bind (value valuep) (data-flow.queue:dequeue fifo)
                    (cond (value
                           (setf good (and good valuep))
                           (cond ((and (<= start value) (< value end))
                                  (incf count)
                                  (setf min-value (min min-value value)
                                        max-value (max max-value value)))
                                 (t
                                  (data-flow.queue:enqueue value fifo))))
                          (t
                           (setf good (and good (not valuep))))))))))))

(test lock-free-fifo
  (labels ((perform (count)
             (let* ((fifo (make-lock-free-fifo))
                    (data (loop
                            for index from 0 below count
                            for start = (* index 5000)
                            for end = (+ start 1000)
                            collect (list start end)))
                    (dequeuers (loop
                                 for index from 0 below count
                                 for (start end) in data
                                 collect (make-dequeuer fifo start end)))
                    (threads (append (loop
                                       for index from 0 below count
                                       for (start end) in data
                                       collect (bordeaux-threads:make-thread
                                                (make-enqueuer fifo start end)))
                                     (loop
                                       for dequeuer in dequeuers
                                       collect (bordeaux-threads:make-thread dequeuer)))))
               (map nil #'bordeaux-threads:join-thread threads)
               (is-true (data-flow.queue:emptyp fifo))
               (multiple-value-bind (value valuep) (data-flow.queue:dequeue fifo)
                 (is-true (null value))
                 (is-false valuep))
               (is-true (loop
                          for dequeuer in dequeuers
                          always (funcall dequeuer t))))))
    (loop
      for count from 1 to 10
      do
         (perform count))))

(test lock-free-fifo-clear
  (let* ((fifo (make-lock-free-fifo)))
    (is-true (data-flow.queue:emptyp fifo))
    (data-flow.queue:enqueue 1 fifo)
    (data-flow.queue:enqueue 2 fifo)
    (is-false (data-flow.queue:emptyp fifo))
    (data-flow.queue:clear fifo)
    (is-true (data-flow.queue:emptyp fifo))
    (is (equalp '(nil nil) (multiple-value-list (data-flow.queue:dequeue fifo))))
    (data-flow.queue:enqueue 3 fifo)
    (is-false (data-flow.queue:emptyp fifo))
    (is (equalp '(3 t) (multiple-value-list (data-flow.queue:dequeue fifo))))
    (is-true (data-flow.queue:emptyp fifo))
    (is (equalp '(nil nil) (multiple-value-list (data-flow.queue:dequeue fifo))))
    (is-true (data-flow.queue:emptyp fifo))))
