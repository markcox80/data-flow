(in-package "DATA-FLOW.LINKED-LIST")

;;;; link

(defgeneric value (link))
(defgeneric next (link))
(defgeneric previous (link))

(defgeneric (setf value) (value link))
(defgeneric (setf next) (value link))
(defgeneric (setf previous) (value link))

(defclass link ()
  ((%value
    :initarg :value
    :accessor value)
   (%next
    :initarg :next
    :accessor next)
   (%previous
    :initarg :previous
    :accessor previous)))

(defmethod value ((link null))
  nil)

(defmethod next ((link null))
  nil)

(defmethod previous ((link null))
  nil)

(defun make-link (value previous next)
  (make-instance 'link
                 :value value
                 :previous previous
                 :next next))

(defun linkp (object)
  (typep object '(or null link)))

;;;; Linked list

(defclass linked-list ()
  ((%back
    :initarg :back
    :initform nil
    :accessor back)
   (%front
    :initarg :front
    :initform nil
    :accessor front)))

(defun make-linked-list ()
  (make-instance 'linked-list))

(defun emptyp (linked-list)
  (check-type linked-list linked-list)
  (and (null (back linked-list))
       (null (front linked-list))))

(defun push-back (object linked-list)
  (check-type linked-list linked-list)
  (cond ((emptyp linked-list)
         (let* ((link (make-link object nil nil)))
           (setf (front linked-list) link
                 (back linked-list) link)))
        (t
         (let* ((current-back (back linked-list))
                (new-back (make-link object current-back nil)))
           (setf (next current-back) new-back
                 (back linked-list) new-back))))
  (values))

(defun pop-back (linked-list)
  (check-type linked-list linked-list)
  (cond ((emptyp linked-list)
         (values nil nil))
        (t
         (let* ((current-back (back linked-list))
                (previous-back (previous current-back)))
           (cond ((null previous-back)
                  (setf (front linked-list) nil
                        (back linked-list) nil))
                 (t
                  (setf (next previous-back) nil
                        (back linked-list) previous-back)))
           (values (value current-back)
                   t)))))

(defun push-front (object linked-list)
  (check-type linked-list linked-list)
  (cond ((emptyp linked-list)
         (let* ((link (make-link object nil nil)))
           (setf (front linked-list) link
                 (back linked-list) link)))

        (t
         (let* ((current-front (front linked-list))
                (new-front (make-link object nil current-front)))
           (setf (previous current-front) new-front
                 (front linked-list) new-front))))
  (values))

(defun pop-front (linked-list)
  (check-type linked-list linked-list)
  (cond ((emptyp linked-list)
         (values nil nil))
        (t
         (let* ((current-front (front linked-list))
                (next-front (next current-front)))
           (cond ((null next-front)
                  (setf (front linked-list) nil
                        (back linked-list) nil))
                 (t
                  (setf (previous next-front) nil
                        (front linked-list) next-front)))
           (values (value current-front)
                   t)))))
