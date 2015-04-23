Timer API for R7RS Scheme
=========================

This library is proposed to the SRFI. See 
[SRFI 120 Timer APIs](http://srfi.schemers.org/srfi-120/)

Timer is one of the important features to implement a practical program.
It is trivial to implement if implementations support thread. However not all
Scheme implementations support SRFI-18 nor POSIX thread model. Some of the
implementations even don't expose mutex. So it is nice to have the interface
to handle timer to hide underlying implementation.

Requirements
============

This library requires the following SRFIs:

- SRFI 18
- SRFI 19
- SRFI 69
- SRFI 114

To run the test script SRFI 64 is also required.

The only implementation I have tested is Sagittarius (0.6.3) and Gauche 
(pre0.9.5). I believe it is trivial to support other implementations.

APIs
====

`(make-timer [error-handler])`

Creates and starts a timer object. The optional argument _error-handler_ 
must be a procedure which accepts one argument. If it is given and when 
a timer task raises an error, then the handler will be invoked and timer
will continue if the _error-handler_ wouldn't raise an error. Otherwise
whenever an error is raised, timer stops and preserves the error.
The error is raised when `timer-cancel!` procedure is called.

`(timer? obj)`

Returns `#t` if given _obj_ is a timer object, otherwise `#f`.

`(timer-cancel! timer)`

Stops the given _timer_. The procedure raises the propagated error if there
is. Once a timer is stopped, it will never be able to start again.

`(timer-schedule! timer thunk when [period])`

Schedules the given _thunk_ as the given _timer_'s task. The _when_ argument
specifies when the task will be started. It can be either timer delta object or
non negative integer. The task is scheduled on the time when the given _when_
passed from the procedure is called.

If the optional argument _period_ is given, which must be either timer delta
object or an integer, then the given task is scheduled as periodical task.
The next task is scheduled by adding _when_ and _period_.

If the _period_ or _when_ is an integer, then it is interpreted as
milliseconds.

The executing order of the same timing tasks are not defined.

The procedure returns timer id which is a readable datum such as an integer.

The _task_ is executed on the dynamic environment where the _timer_ is
created.

`(timer-reschedule! timer id when [period])`

Reschedules the task associated to the given _id_ on the given _timer_.
The _when_ and _period_ arguments are the same as `timer-schedule!`.

Thus to cancel the periodical task, you can specify 0 as _period_ argument.

The procedure returns given _id_.

`(timer-task-remove! timer id)`

Removes the task associated to the given _id_ on the given _timer_. It
returns `#t` if a task is removed, otherwise `#f`.

`(timer-task-exists? timer id)`

Returns `#t` if a task associated to the given _id_ exists, otherwise `#f`.

`(make-timer-delta n unit)`

Creates a timer delta object. _n_ must be an integer and _unit_ must be
a symbol which represents the time unit. Implementations must support
the following units:

- `h` : hour
- `m` : minute
- `s` : second
- `ms` : millisecond
- `us` : microsecond
- `ns` : nanosecond

And may support other unit.

`(timer-delta? obj)`

Returns `#t` if given _obj_ is a timer delta object, otherwise `#f`.

Implementation notes
====================

A task should be able to cancel or reschedule other tasks. But it should
not be able to cancel or reschedule itself.

If a task is rescheduled whenever it's executed, the timer doesn't stop its
execution. It is rescheduled but the current execution will be continued.
