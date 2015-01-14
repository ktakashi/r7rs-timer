Timer API for R7RS Scheme
=========================

This library will be proposed to SRFI soon.

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

The only implementation I have tested is Sagittarius (0.6.0). I believe
it is trivial to support other implementations.

APIs
====

`(make-timer [error-handler])`

Creates a timer object. The optional argument _error-handler_ must be a
procedure which accepts one argument. If it is given and when a timer
task raises an error, then the handler will be invoked and timer will
continue if the _error-handler_ wouldn't raise an error. Otherwise
whenever an error is raised, timer propagates the error.

`(timer? obj)`

Returns `#t` if given _obj_ is a timer object.

`(timer-start! timer)`

Starts the given _timer_. Timer won't do any task unless it's started.
If one or more tasks are scheduled before the timer is started and
the scheduled time is passed, then timer invokes the tasks.

`(timer-stop! timer)`

Stops the given _timer_. The procedure raises the propagated error if there
is. Once a timer is stopped, it will never be able to start again.

`(timer-schedule! timer thunk when [period])`

Schedules the given _thunk_ as the given _timer_'s task. The _when_ argument
specifies when the task will be started. It can be either time object or
non negative integer. If the _when_ is a time object, then the task is
scheduled on that time. If the _when_ is an integer, then the task is
scheduled on passed number milliseconds later.

If the optional argument _period_ is given and if it's a positive number,
then the given task is scheduled as periodical task. The number is interpreted
as milliseconds.

The procedure returns timer id which is an integer.

`(timer-reschedule! timer id when [period])`

Reschedules the task associated to the given _id_ on the given _timer_.
The _when_ and _period_ arguments are the same as `timer-schedule!`.

Thus to cancel the periodical task, you can specify 0 as _period_ argument.

The procedure returns given _id_.

`(timer-remove! timer id)`

Removes the task associated to the given _id_ on the given _timer_. It
returns `#t` if a task is removed, otherwise `#f`.

`(timer-exists? timer id)`

Returns `#t` if a task associated to the given _id_ exists, otherwise `#f`.
