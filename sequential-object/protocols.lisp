(in-package "DATA-FLOW.SEQUENTIAL-OBJECT")

(defgeneric linearized-apply (function sequential-object))

(defmacro linearize (sequential-object &body body)
  `(linearized-apply (lambda ()
                       ,@body)
                     ,sequential-object))
