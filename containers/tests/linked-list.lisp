(in-package "DATA-FLOW.LINKED-LIST.TESTS")
(5am:in-suite linked-list-tests)

(test make-link
  (let* ((link (make-link 1 2 3)))
    (is (eql 1 (value link)))
    (is (eql 2 (previous link)))
    (is (eql 3 (next link)))

    (setf (value link) 4
          (previous link) 5
          (next link) 6)

    (is (eql 4 (value link)))
    (is (eql 5 (previous link)))
    (is (eql 6 (next link)))))

(test link
  (let* ((link (make-link 1 2 3)))
    (is-true (linkp link))
    (is-true (linkp nil))
    (is-false (linkp 1))))

(test null-link
  (is-true (null (value nil)))
  (is-true (null (previous nil)))
  (is-true (null (next nil)))

  (signals error (setf (value nil) 1))
  (signals error (setf (previous nil) 2))
  (signals error (setf (next nil) 3)))

(test make-linked-list
  (let* ((list (make-linked-list)))
    (is-true (emptyp list))
    (is-true (null (front list)))
    (is-true (null (back list)))))

(test push-front/empty
  (let* ((list (make-linked-list)))
    (push-front 1 list)
    (let* ((front (front list))
           (back (back list)))
      (is-false (null front))
      (is (eql front back))
      (is (eql 1 (value front)))
      (is (null (previous front)))
      (is (null (next front))))))

(test push-front/single
  (let* ((list (make-linked-list)))
    (push-front 1 list)
    (push-front 2 list)
    (let* ((front (front list))
           (back (back list)))
      (is-false (eql front back))
      (is (eql 2 (value front)))
      (is-true (null (previous front)))
      (is (eql back (next front)))

      (is (eql 1 (value back)))
      (is (eql front (previous back)))
      (is-true (null (next back))))))

(test pop-front/empty
  (let* ((list (make-linked-list)))
    (multiple-value-bind (value value?) (pop-front list)
      (is-true (null value))
      (is-true (not value?))
      (is-true (null (front list)))
      (is-true (null (back list))))))

(test pop-front/single
  (let* ((list (make-linked-list)))
    (push-front 1 list)
    (multiple-value-bind (value value?) (pop-front list)
      (is (eql 1 value))
      (is (eql t value?))
      (is-true (null (front list)))
      (is-true (null (back list))))))

(test pop-front/two
  (let* ((list (make-linked-list)))
    (push-front 1 list)
    (push-front 2 list)
    (multiple-value-bind (value value?) (pop-front list)
      (is (eql 2 value))
      (is (eql t value?))
      (let* ((front (front list))
             (back (back list)))
        (is (eql front back))
        (is (eql 1 (value front)))
        (is (null (previous front)))
        (is (null (next front)))))))

(test pop-front/three
  (let ((list (make-linked-list)))
    (push-front 1 list)
    (push-front 2 list)
    (push-front 3 list)

    (multiple-value-bind (value value?) (pop-front list)
      (is (eql 3 value))
      (is (eql t value?))
      (let* ((front (front list))
             (back (back list)))
        (is (eql 2 (value front)))
        (is-true (null (previous front)))
        (is (eql back (next front)))

        (is (eql 1 (value back)))
        (is (eql front (previous back)))
        (is-true (null (next back)))))))

(test push-back/empty
  (let* ((list (make-linked-list)))
    (push-back 1 list)
    (let* ((front (front list))
           (back (back list)))
      (is (eql front back))
      (is (eql 1 (value front)))
      (is-true (null (previous front)))
      (is-true (null (next front))))))

(test push-back/single
  (let* ((list (make-linked-list)))
    (push-back 1 list)
    (push-back 2 list)
    (let* ((front (front list))
           (back (back list)))
      (is-false (eql front back))

      (is (eql 1 (value front)))
      (is-true (null (previous front)))
      (is (eql back (next front)))

      (is (eql 2 (value back)))
      (is (eql front (previous back)))
      (is-true (null (next back))))))

(test pop-back/empty
  (let* ((list (make-linked-list)))
    (multiple-value-bind (value value?) (pop-back list)
      (is-true (null value))
      (is-false value?)
      (is-true (null (front list)))
      (is-true (null (back list))))))

(test pop-back/single
  (let* ((list (make-linked-list)))
    (push-back 1 list)
    (multiple-value-bind (value value?) (pop-back list)
      (is (eql 1 value))
      (is-true value?)
      (is-true (null (front list)))
      (is-true (null (back list))))))

(test pop-back/two
  (let* ((list (make-linked-list)))
    (push-back 1 list)
    (push-back 2 list)
    (multiple-value-bind (value value?) (pop-back list)
      (is (eql 2 value))
      (is-true value?)
      (let* ((front (front list))
             (back (back list)))
        (is (eql front back))
        (is (eql 1 (value front)))
        (is-true (null (previous front)))
        (is-true (null (next front)))))))

(test pop-back/three
  (let* ((list (make-linked-list)))
    (push-back 2 list)
    (push-back 3 list)
    (push-front 1 list)
    (multiple-value-bind (value value?) (pop-back list)
      (is (eql 3 value))
      (is-true value?)
      (let* ((front (front list))
             (back (back list)))
        (is-false (eql front back))
        (is (eql 1 (value front)))
        (is-true (null (previous front)))
        (is-true (eql back (next front)))

        (is (eql 2 (value back)))
        (is (eql front (previous back)))
        (is-true (null (next back)))))))

(test empty
  (let* ((list (make-linked-list)))
    (is-true (emptyp list))
    (push-back 1 list)
    (is-false (emptyp list))
    (push-front 2 list)
    (is-false (emptyp list))
    (is (eql 1 (pop-back list)))
    (is-false (emptyp list))
    (is (eql 2 (pop-back list)))
    (is-true (emptyp list))
    (is-true (null (front list)))
    (is-true (null (back list)))))
