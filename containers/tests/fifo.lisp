(in-package "DATA-FLOW.FIFO.TESTS")
(5am:in-suite fifo-tests)

(defun dequeue-test (expected-object expected-object? queue)
  (multiple-value-bind (object object?) (data-flow.queue:dequeue queue)
    (and (equalp expected-object object)
         (eql expected-object? object?))))

(test fifo-test
  (let* ((fifo (data-flow.fifo:make-fifo)))
    (is (dequeue-test nil nil fifo))
    (data-flow.queue:enqueue fifo 1)
    (data-flow.queue:enqueue fifo 2)
    (is (dequeue-test 1 t fifo))
    (is (dequeue-test 2 t fifo))
    (is (dequeue-test nil nil fifo))
    (data-flow.queue:enqueue fifo 3)
    (is (dequeue-test 3 t fifo))
    (is (dequeue-test nil nil fifo))
    (is (dequeue-test nil nil fifo))))

(test fifo-test/emptyp
  (let* ((fifo (data-flow.fifo:make-fifo)))
    (is-true (data-flow.queue:emptyp fifo))
    (data-flow.queue:enqueue fifo 1)
    (is-false (data-flow.queue:emptyp fifo))
    (data-flow.queue:enqueue fifo 2)
    (is-false (data-flow.queue:emptyp fifo))
    (dequeue-test 1 t fifo)
    (is-false (data-flow.queue:emptyp fifo))
    (dequeue-test 2 t fifo)
    (is-true (data-flow.queue:emptyp fifo))))

(test clear-test
  (let* ((fifo (data-flow.fifo:make-fifo)))
    (data-flow.queue:enqueue fifo 1)
    (data-flow.queue:enqueue fifo 2)
    (is-false (data-flow.queue:emptyp fifo))
    (data-flow.queue:clear fifo)
    (is-true (data-flow.queue:emptyp fifo))
    (multiple-value-bind (item item?) (data-flow.queue:dequeue fifo)
      (is-true (null item))
      (is-true (null item?)))
    (data-flow.queue:enqueue fifo 3)
    (multiple-value-bind (item item?) (data-flow.queue:dequeue fifo)
      (is (eql 3 item))
      (is-true item?))))
