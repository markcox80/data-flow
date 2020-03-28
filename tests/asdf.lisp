(in-package "DATA-FLOW.TESTS")

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system "data-flow/tests"))))
  (5am:run! 'data-flow.tests:all-data-flow-tests)
  (values))
