Data Flow for Common Lisp
-------------------------

The data flow system provides an interface for building applications
where data "flows" from one component to another via events and/or
ports. The logic of a component is scheduled to be executed when an
event has arrived, data is available on a port or there is new
capacity in a connection. Applications authored in this way require
minimal effort to take advantage of multi-threaded execution
environments.

The goals for the data flow system are:
- There are no assumptions on the execution environment i.e. all
  applications written using the API can progress in either single
  threaded or multi-threaded execution environments with no code
  changes.
- Users have full control over when a component is executed.
- It is impossible for the logic of a component to be executed by two
  or more threads simultaneously when using the events and/or ports
  interface.
- Component authors do not need to consider concurrent access to
  objects when immutable data is shared between components.
- It must be possible for components to implement non-blocking I/O
  efficiently.

The data flow system supports the following common lisp
implementations:
1. Implementations with no thread support.
2. Multi-threaded implementations supported by [Bordeaux
   Threads](https://common-lisp.net/project/bordeaux-threads/).
3. SBCL's [compare and swap](http://www.sbcl.org/manual/#Atomic-Operations)
   support if available.

The data flow system provides abstractions for the following:
1. [Scheduling](doc/schedulers.md)
2. [Components](doc/components.md)
3. [Ports](doc/ports.md)
4. [Sequential Objects](doc/sequential-objects.md)

Each component is assigned to a single scheduler and more than one
scheduler can be used simultaneously.

The amount of contention in the scheduler implementations is bounded
by the number of threads assigned to each scheduler. The amount of
contention in the standard component implementation is bounded by the
maximum number of threads interacting with the component or the number
of threads assigned to the component's scheduler.

# Example
Below is an example application involving two components which
communicate with each other using ports. One component (class
`map-list-component`) sends values from a list to another component
which prints the received values (class `print-component`).

```lisp
;;; Load the data flow system.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "data-flow"))

;;; Define the application package.

(defpackage "DATA-FLOW-EXAMPLE"
  (:use "COMMON-LISP"))
(in-package "DATA-FLOW-EXAMPLE")

;;; MAP-LIST-COMPONENT
;;;
;;; The MAP-LIST-COMPONENT writes values from a given list.

(defclass map-list-component (data-flow:standard-component)
  ((%remaining-list :initarg :list
                    :accessor remaining-list)
   (%output-port :initform (data-flow:make-output-port)
                 :reader output-port)))

(defmethod data-flow:run ((component map-list-component))
  (loop
    while (cond ((remaining-list component)
                 (let* ((value (pop (remaining-list component))))
                   (data-flow:write-value-case (value (output-port component))
                     (data-flow:success
                      t)

                     (data-flow:no-space-available
                      (push value (remaining-list component))
                      nil)

                     (data-flow:disconnected
                      nil))))

                (t
                 (data-flow:disconnect-port (output-port component))
                 nil))))

;;; PRINT-COMPONENT
;;;
;;; A component which reads values from the input port and prints them
;;; to *STANDARD-OUTPUT*.

(defclass print-component (data-flow:standard-component)
  ((%input-port :initform (data-flow:make-input-port)
                :reader input-port)))

(defmethod data-flow:run ((component print-component))
  (loop
    while (data-flow:read-value-case (value (input-port component))
            (data-flow:success
             (print value)
             (force-output)
             t)

            ((data-flow:no-value-available data-flow:disconnected)
             nil))))

;;; Construct the application.
(defun main (scheduler)
  (let* ((map-list-component (make-instance 'map-list-component
                                            :list (list 1 2 3 4 5 6 7 8 9 10)
                                            :scheduler scheduler))
         (print-component (make-instance 'print-component
                                         :scheduler scheduler)))
    ;; Connect the ports together.
    (data-flow:connect-ports map-list-component (output-port map-list-component)
                             print-component (input-port print-component))

    ;; Schedule the map-list-component to get things moving.
    (data-flow:schedule scheduler map-list-component)

    ;; Execute tasks until finished.
    (data-flow:execute scheduler)))

;; Execute the application using a sequential scheduler.
(main (data-flow.sequential-scheduler:make-sequential-scheduler))

;; Execute the application using a thread pool.
(main (data-flow.thread-pool:make-thread-pool 2))
```

# Installation
The system depends on the following external projects available from
(quicklisp)[https://www.quicklisp.org/beta/]:
1. [Alexandria](https://common-lisp.net/project/alexandria/)
2. [Boreeaux Threads](https://common-lisp.net/project/bordeaux-threads/)
3. [FiveAM](https://common-lisp.net/project/fiveam/) (only required for unit tests)

The data flow system uses
[ASDF](https://common-lisp.net/project/asdf/) to manage building.
