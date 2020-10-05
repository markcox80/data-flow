Schedulers
----------

A scheduler is responsible for executing a set of tasks. The following
schedulers are provided in the data flow system:
- [Sequential Scheduler](# Sequential Scheduler)
- [Thread Pool Scheduler](# Thread Pool Scheduler)
- [Resource Scheduler](# Resource Schduler)

# Scheduler Protocol
All schedulers implement the scheduler protocol in the `DATA-FLOW`
package.

Each task is referred to as a `RUNNABLE` and all runnables are
executed by a scheduler using the `RUN` generic function.

A default `RUN` method is provided for function objects which accept
no parameters.

Runnables can be scheduled using the `SCHEDULE` function e.g.
```lisp
(schedule scheduler (lambda () (print "here")))
```

A scheduler is always in one of the following states:
- executing
- executing1
- not executing and unclean
- not executing and cleaned

# Sequential Scheduler

# Thread Pool Scheduler

# Resource Scheduler
