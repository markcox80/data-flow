Sequential Objects
------------------

A sequential object is a protocol inspired by the Sequential Object
interface described in the book [The Art of Multiprocessor
Programming](https://dl.acm.org/doi/book/10.5555/2385452).

The interface ensures all function applications are linearised i.e. it
is impossible for two or more threads to be executing functions
simultaneously.

Linearised applications of functions is performed by the function

```lisp
(defgeneric data-flow.sequential-object:linearized-apply (function sequential-object))
```

where `FUNCTION` is the function to linearize and `SEQUENTIAL-OBJECT`
is an instance of the class
`DATA-FLOW.SEQUENTIAL-OBJECT:SEQUENTIAL-OBJECT`.

The macro `DATA-FLOW.SEQUENTIAL-OBJECT:LINEARIZE` provides a more
convenient form of invoking
`DATA-FLOW.SEQUENTIAL-OBJECT:LINEARIZED-APPLY`.

```lisp
(data-flow.sequential-object:linearize sequential-object
  (print "here"))
```

Note that the thread which executes `FUNCTION` may or may not be the
same thread which called the function `LINEARIZED-APPLY`. Thus any
exit points for a non local transfer of control e.g. [go
tags](http://www.lispworks.com/documentation/HyperSpec/Body/s_tagbod.htm)
or
[restarts](http://www.lispworks.com/documentation/HyperSpec/Body/09_adb.htm)
will not be available. Note that any error signalled during the
execution of function will be re-signalled in the calling thread.
