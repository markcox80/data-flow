Components
----------

Applications written using the data flow system are composed of
components which implement the data flow component interface. A
component's logic is executed by a scheduler by at most one thread at
any given time.

Components communicate with other components using events. All events
are sent using `DATA-FLOW:ENQUEUE-EVENT` and the component's logic is
scheduled for execution if and only if the component is /stopped/
i.e. it is not executing/running nor scheduled.

When scheduling a component for execution, a component implementation
must schedule the closure returned by
`DATA-FLOW:MAKE-COMPONENT-LAMBDA` rather than scheduling the component
itself.

Upon invocation, the closure first processes all of the events
received by the component using `DATA-FLOW:PROCESS-ALL-EVENTS`. It
then applies `DATA-FLOW:RUN` to the component. The component's
`DATA-FLOW:RUN` method must call `DATA-FLOW:PROCESS-ALL-EVENTS` to
obtain any new data sent to the component during an invocation of
`DATA-FLOW:RUN`.

Upon completion of the component's `DATA-FLOW:RUN` method, the closure
must call the predicate `DATA-FLOW:REQUIRES-EXECUTION-P` to check if
any new events have not been processed by the component. The component
will be rescheduled for execution if the predicate returns non `NIL`.

Each event object received by a component must have an applicable
`DATA-FLOW:PROCESS-EVENT` method. This generic function is applied to
each received event by `DATA-FLOW:PROCESS-ALL-EVENTS`

The generic functions `DATA-FLOW:EXECUTION-STATE` and
`DATA-FLOW:COMPARE-AND-CHANGE-EXECUTION-STATE` ensure that at most one
thread can execute the logic of the component. The execution state of
a component can be one of `:STOPPED`, `:SCHEDULED` or `:RUNNING`. The
function `DATA-FLOW:EXECUTION-STATE` returns the current state of the
component, and the function
`DATA-FLOW:COMPARE-AND-CHANGE-EXECUTION-STATE` will change the state
of the component if and only if the component's current state is equal
to a specific value.

The scheduler used by a component can be accessed using the
`DATA-FLOW:SCHEDULER` function.

Users are able to use the dynamic variable `DATA-FLOW:*SCHEDULER*` to
specify the default scheduler for a component.

Implementations of `DATA-FLOW:ENQUEUE-EVENT`,
`DATA-FLOW:EXECUTION-STATE` and
`DATA-FLOW:COMPARE-AND-CHANGE-EXECUTION-STATE` must linearize all
concurrent access to the component. All other operators are assumed to
be invoked during an invocation of the closure returned by
`DATA-FLOW:MAKE-COMPONENT-LAMBDA`.

# Classes
An object which implements the component protocol must be a subclass
of `DATA-FLOW:COMPONENT`.

The data flow system provides the class `DATA-FLOW:BASIC-COMPONENT`
which implements the component protocol above.

The data flow system provides the class `DATA-FLOW:STANDARD-COMPONENT`
which is a subclass of `DATA-FLOW:BASIC-COMPONENT` and has support for
all of the other functionality provided by the data flow system
(e.g. [ports.md](ports)).
