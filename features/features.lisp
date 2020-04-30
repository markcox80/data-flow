(in-package "DATA-FLOW.FEATURES")

(define-condition feature-unavailable-error (error)
  ((%pathname :initarg :pathname
              :initform (or *compile-file-pathname*
                            *load-pathname*)
              :reader feature-unavailable-error-pathname)
   (%feature :initarg :feature
             :reader feature-unavailable-error-feature))
  (:report (lambda (condition stream)
             (let* ((feature (feature-unavailable-error-feature condition)))
               (format stream "Cannot load/compile file ~A as the feature ~A:~A is unavailable."
                       (feature-unavailable-error-pathname condition)
                       (package-name (symbol-package feature))
                       (symbol-name feature))))))


;;;; Threads

#+thread-support
(pushnew 'threads *features*)

(define-condition threads-unavailable-error (feature-unavailable-error)
  ()
  (:default-initargs
   :feature 'threads))
