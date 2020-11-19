Ports
-----

A port in the data flow system represents one end of a unidirectional
communication channel between two components. The implementation is
built on top of the events interface defined for
[components](components.md).

All operations on ports are non blocking to ensure the application
progresses.

The default implementation of the ports protocol in the `DATA-FLOW`
system requires components to be a subclass of
`DATA-FLOW:STANDARD-PORT-COMPONENT-MIXIN`.

There are two classes of ports, `DATA-FLOW:INPUT-PORT` and
`DATA-FLOW:OUTPUT-PORT`, representing the input and output end of the
channel respectively. Instances of these classes are created using the
functions `DATA-FLOW:MAKE-INPUT-PORT` and
`DATA-FLOW:MAKE-OUTPUT-PORT`.

# Connecting

A communication channel is constructed by connecting an input port to
an output port using the function
`DATA-FLOW:CONNECT-PORTS`
```lisp
(defgeneric data-flow:connect-ports (component1 port1 component2 port2 &key total-space &allow-other-keys))
```

The arguments `port1` and `port2` are the input and output ports to
connect.

When the state of `port1` changes, the component `component2` will be
scheduled for execution. Similarly, when the state of `port2` changes,
the component `component1` will be scheduled for execution.

All connections have finite capacity which is specified by the
`TOTAL-SPACE` argument of `DATA-FLOW:CONNECT-PORTS`. If no value is
provided, the value of `DATA-FLOW:*DEFAULT-TOTAL-SPACE*` is used.

A connection can be severed by applying `DATA-FLOW:DISCONNECT-PORT` to
a port.

All operations on a port attempt to query or change its
state. Notifications of changes in state are automatically sent to the
other port via [component events](components.md). All port operations
must invoke `DATA-FLOW:PROCESS-EVENTS` on the component associated
with the port i.e. specified at `DATA-FLOW:CONNECT-PORTS`.

# Output Ports

There are three possible states an output port can be in when an
operation attempts to query or change a port's state:
1. The port is connected and no space is available in the channel for a new value.
2. The port is disconnected.
3. The port is connected and space is available in the channel for a new value.

The example below writes the value `1` to the port `output-port` using
the macro function `DATA-FLOW:WRITE-VALUE-CASE`.

```lisp
(data-flow:write-value-case (1 output-port)
  (data-flow:no-space-available
   (print "No space available."))

  (data-flow:disconnected
   (print "Port has been disconnected."))

  (data-flow:success
   (print "Value written.")))
```

If no space is available in the channel for a new value, then
`WRITE-VALUE-CASE` will execute the body of the clause with the key
`DATA-FLOW:NO-SPACE-AVAILABLE`. If the port is disconnected, then
`WRITE-VALUE-CASE` will execute the body of the
`DATA-FLOW:DISCONNECTED` clause. If the value is written, then the
`DATA-FLOW:SUCCESS` clause will be executed.

If no `DATA-FLOW:NO-SPACE-AVAILABLE` clause is specified, then an
error with class `DATA-FLOW:NO-SPACE-AVAILABLE-ERROR` is signalled.

If no `DATA-FLOW:DISCONNECTED` clause is specified, then an error with
class `DATA-FLOW:PORT-DISCONNECTED-ERROR` is signalled.

The function `DATA-FLOW:WRITE-VALUE` can also be used to write values
to a port.
```lisp
(defgeneric write-value (value port &key errorp no-space-value disconnected-value &allow-other-keys))
```

The predicate `DATA-FLOW:SPACE-AVAILABLE-P` tests if space is
available in a connection. The total amount of space available can be
queried using `DATA-FLOW:AVAILABLE-SPACE`.

# Input Ports

There are three possible states an input port can be in when an
operation attempts to query or change an input port's state:
1. The port is connected and no value is available for reading.
2. The port is disconnected.
3. The port is connected and a value is available for reading.

The example below reads a value from `input-port` using the macro
function `DATA-FLOW:READ-VALUE-CASE`.

```lisp
(data-flow:read-value-case (value input-port)
  (data-flow:no-value-available
   (print "No value available."))

  (data-flow:disconnected
   (print "The port is disconnected."))

  (data-flow:success
   (format t "Read the value ~A from the input port." value)))
```

If no value is available in the channel, then `READ-VALUE-CASE` will
execute the body of the `DATA-FLOW:NO-VALUE-AVAILABLE` clause. If the
port is disconnected, then `READ-VALUE-CASE` will execute the body of
the `DATA-FLOW:DISCONNECTED` clause. If a value is available, then the
`DATA-FLOW:SUCCESS` clause will be executed.

If no `DATA-FLOW:NO-VALUE-AVAILABLE` clause is specified, then an
error with class `DATA-FLOW:NO-VALUE-AVAILABLE-ERROR` will be
signalled.

If no `DATA-FLOW:DISCONNECTED` clause is specified, then an error with
class `DATA-FLOW:PORT-DISCONNECTED-ERROR` will be signalled.

The function `DATA-FLOW:READ-VALUE` can also be used to read values
from a port.

```lisp
(defgeneric read-value (port &key errorp no-value-value disconnected-value &allow-other-keys))
```
