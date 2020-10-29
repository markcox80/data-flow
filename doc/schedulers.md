Schedulers
----------

A scheduler in the data flow system is responsible for executing
tasks. Users are permitted to use more than one scheduler in their
applications.

The following schedulers are provided in the data flow system:
- [Sequential Scheduler](# Sequential Scheduler)
- [Thread Pool Scheduler](# Thread Pool Scheduler)
- [Resource Scheduler](# Resource Schduler)

Note that the above schedulers exist even if the lisp machine does not
support threading.

# Scheduler Protocol
All schedulers implement the scheduler protocol in the `DATA-FLOW`
package.

An individual task is referred to as a `RUNNABLE` and all runnables
must provide a `RUN` method. A `RUN` method is provided for `FUNCTION`
objects which assumes that the function object requires no arguments.

Runnables can be scheduled using the `SCHEDULE` function
```lisp
(schedule scheduler (lambda () (print "here")))
```

Logically, all schedulers contain two queues, a scheduled queue and an
execution queue. Upon starting a scheduler using `START1` or `START`,
all runnables in the scheduled queue are moved to the execution
queue. Once started, the user calls `WAIT-UNTIL-FINISHED` to wait for
the runnables in the execution queue to complete or a specified wall
clock time to elapse.

Note that it is unspecified when runnables start executing, but
runnables must have started executing when `START1` or `START`
followed by `WAIT-UNTIL-FINISHED` has been applied to the scheduler.

If a scheduler is started using `START1` then any runnable scheduled
after invocation of `START1` will be added to the scheduled queue and
will only be executed the next time the scheduler is started and
`WAIT-UNTIL-FINISHED` is applied to the scheduler.

If a scheduler is started using `START` then any runnable scheduled
after invocation of `START` will be added to the execution queue.

The function `WAIT-UNTIL-FINISHED` returns when there are no more
tasks in the execution queue or a specified wall clock time has
elapsed. The function returns two values, `FINISHED?` and `NEW?`,
which indicate if the execution queue is empty and if any new tasks
were scheduled respectively.

Note that it is possible for `WAIT-UNTIL-FINISHED` to perform a non
local transfer of control if a runnable signals an error. More details
are available in the [Errors](## Errors) Section.

Any resources allocated by the scheduler can be reclaimed by executing
the `CLEAN` function.

The functions `EXECUTE1` and `EXECUTE` provide a convenient way of
starting, waiting for completion and cleaning up schedulers.

## Errors

The `*ON-ERROR*` dynamic variable is used to control the behaviour of
the scheduler when a runnable signals an error. Valid values for this
variable are the keywords:
- `:START1` (default)
- `:DEBUG`
- `:IGNORE`
- `:WARN-AND-IGNORE`
- `:WARN-AND-START1`

A value of `:START1` specifies that the scheduler is to handle and
store the error condition and then enter a state equivalent to
applying `START1` to the scheduler i.e. any runnables scheduled after
handling the error will be added to the scheduled queue. Once all
tasks in the execution queue are finished, an invocation of
`WAIT-UNTIL-FINISHED` will (re)signal the stored error to allow the
application to be notified of the error. It is unspecified which error
will be (re)signalled if more than one error is signalled.

A value of `:DEBUG` requires the scheduler to invoke the debugger
using `CL:INVOKE-DEBUGGER`.

A value of `:IGNORE` requires the scheduler to ignore the error and
continue execution.

A value of `:WARN-AND-IGNORE` requires the scheduler to perform the
same actions as in `:IGNORE` and output a message to `*ERROR-OUTPUT*`.

A value of `:WARN-AND-START1` requires the scheduler to perform the
same actions as `:START1` and output a message to
`*ERROR-OUTPUT*`.

## Blocking

Runnables which perform I/O can use the predicate `BLOCKING-ALLOWED-P`
to obtain permission from the scheduler to block.

# Sequential Scheduler

# Thread Pool Scheduler

# Resource Scheduler
