#-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This file is only for SBCL."))

(in-package "DATA-FLOW.UTILS")

#+data-flow.features:compare-and-set
(defmacro compare-and-set (place current-value new-value)
  (alexandria:with-gensyms (tmp-current)
    `(let* ((,tmp-current ,current-value))
       (eq (sb-ext:compare-and-swap ,place ,tmp-current ,new-value)
           ,tmp-current))))
