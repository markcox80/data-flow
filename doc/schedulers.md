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

Runnables are scheduled using the `SCHEDULE` function
```lisp
(schedule scheduler (lambda () (print "here")))
```

Logically, all schedulers contain two queues, a scheduled queue and an
execution queue. Upon starting a scheduler using `START1` or `START`,
all runnables in the scheduled queue are moved to the execution
queue. Once started, the user calls `WAIT-UNTIL-FINISHED` to wait for
the runnables in the execution queue to complete or a specified wall
clock time to elapse.

It is unspecified when runnables start executing, however, runnables
must have started executing when `START1` or `START` followed by
`WAIT-UNTIL-FINISHED` has been applied to the scheduler.

If a scheduler is started using `START1` then any runnable scheduled
after invocation of `START1` will be added to the scheduled queue and
executed the next time the functions `START1` or `START` followed by
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
same actions as in `:IGNORE` and output a message to `CL:*ERROR-OUTPUT*`.

A value of `:WARN-AND-START1` requires the scheduler to perform the
same actions as `:START1` and output a message to
`CL:*ERROR-OUTPUT*`.

## Blocking

Runnables which perform I/O can use the predicate `BLOCKING-ALLOWED-P`
to obtain permission from the scheduler to block.

## Sequential and Parallel
A scheduler which is a subclass of `DATA-FLOW:SEQUENTIAL-SCHEDULER`
has at most one runnable executing.

A scheduler which is a subclass of `DATA-FLOW:PARALLEL-SCHEDULER` is
able to execute more than one runnable simultaneously. The number of
threads available to the scheduler can be obtained using the function
`DATA-FLOW:NUMBER-OF-THREADS`.

# Sequential Scheduler
The scheduler returned by the function
`DATA-FLOW.SEQUENTIAL-SCHEDULER:MAKE-SEQUENTIAL-SCHEDULER` is the
basic implementation of the scheduler protocol. It assumes that the
underlying common lisp runtime has no thread support.

# Thread Pool Scheduler
The thread pool scheduler allows multiple runnables to be executed
simultaneously if the common lisp runtime has thread support. If no
thread support is available, the scheduler returned is a subclass of
the sequential scheduler.

The function `DATA-FLOW.THREAD-POOL:MAKE-THREAD-POOL` creates a thread
pool scheduler which uses a specified number of threads.

# Resource Scheduler
The resource scheduler is a thread pool scheduler which executes
runnables if and only if sufficient resources are available.

The function `DATA-FLOW.RESOURCE-SCHEDULER:MAKE-RESOURCE-SCHEDULER`
creates a resource scheduler using the specified number of threads,
the specified available resource (see below) and a function which
computes the required resources for a runnable.

The resource maintained by a resource scheduler must implement the
following resource pool protocol:
- `(DATA-FLOW.RESOURCE-SCHEDULER:TEST-RESOURCES-P RESOURCE-POOL RESOURCES)`
- `(DATA-FLOW.RESOURCE-SCHEDULER:TEST-AND-CLAIM-RESOURCES RESOURCE-POOL RESOURCES)`
- `(DATA-FLOW.RESOURCE-SCHEDULER:RETURN-RESOURCES RESOURCE-POOL RESOURCES)`

The predicate `TEST-RESOURCES-P` returns non nil if `RESOURCES` can be
removed from the `RESOURCE-POOL`.

The function `TEST-AND-CLAIM-RESOURCES` returns a new resource pool if
`RESOURCES` can be removed from the pool. The function returns `NIL`
otherwise.

The function `RETURN-RESOURCES` returns a new resource pool which has
`RESOURCES` added to the pool.

An implementation of the resource pool protocol is provided for a
resource which can be modelled using a non negative real.

# Implementing a new scheduler
Users are able to write their own scheduler if the above
implementations are unsatisfactory.

One requirement of all schedulers is that the scheduled or execution
queue must not be equivalent to a last-in-first-out queue. Such a
strategy can result in applications failing to progress.
