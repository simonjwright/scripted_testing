# Scripted Testing

## Introduction

To set the scene, the Ada software under test (SUT) forms part of a system. Generally, the system will be constructed using a layered approach, and the other parts of the system that interact with the SUT are higher-level (which can call interface subprograms presented by the SUT) or lower-level (whose interfaces can be called by the SUT, or which can provide callbacks to the SUT).

This package is intended for testing at a level between unit testing (using, for example, [AUnit](https://github.com/AdaCore/aunit)) and integration testing (using real hardware). Unit testing tends to be fragile, requiring a lot of rework for even minor functional changes in the requirements.

The scripting language supported is [Tcl](http://www.tcl.tk), and specifically the Ada binding [Tash](https://github.com/simonjwright/tcladashell). The reason for choosing Tcl rather than Python or Lua is that Tcl's interface is entirely string-based; this is important, considering the need to specify values of enumerated types.

The package provides facilities to write new commands in Tcl to

* call the interface subprograms presented by the SUT,

* set up values to be returned by calls the SUT makes to lower-level system components,

* call callbacks provided by lower-level system components,

* delay for appropriate periods, and

* check that the proper calls have been made to lower-level system components, with the required values.

It's assumed that the interface subprograms of the lower-level subsystems are stubbed so that:

* `in` and `in out` parameters can be recorded for later checking,

* `out` (and `in out`) parameter values and function `return` values can be provided to be returned to the SUT,

* exceptions can be raised when required,

* the number of calls to the subprogram can be checked.

The [stubbing facilities](https://simonjwright.github.io/coldframe/stubs.html) of [ColdFrame](https://github.com/simonjwright/coldframe) meet the above requirements.

## Description

The components of the package are _Commands_, _Actions_, and an _Action Queue_.

### Commands

A Command implements a Tcl command.

It creates an Action to be executed at run time, and posts it on the Action Queue.

<a href="#provided-commands">Some commands</a> are provided by this package. Other commands are to be provided to support the specific application to be tested: typically, <tt>call\_<i>procedure</i> <i>param1</i> <i>param2</i> ...</tt>  (where the parameters are those required by _procedure_) and <tt>check\_<i>subprogram</i> <i>parameter</i> <i>value</i></tt> (to check the value passed to _procedure_ in _parameter_ on the last call).

<!-- XXX do they need to know all this? -->
Commands have to be `Register`ed with this package, because once Tcl has been started (using `Start`, which doesn't return) no more Commands can be added. Registration would normally be done during elaboration of the package in which the Command is defined (see `test/test-first.adb`).

### Actions

An Action carries the data required to enact the command at run time.

When it is executed, it performs the required action.

* If all is well, it completes normally.

* If some condition fails, it raises an exception (`Execution_Failure`) with a message stating the problem.

* Any other exceptions are propagated.

### Action Queue

When the Action Queue is started (using the `go` command), it repeatedly picks the next Action and executes it, until either the end of the queue is reached (which implies that the script has succeeded) or an exception is propagated (which implies that the script has failed).

## <a name="provided-commands">Provided commands</a>

The commands provided by this package are

<dl>

<dt><tt>echo "<i>string</i>"</tt> <dd>outputs <tt><i>string</i></tt> to the terminal at run time. Useful to report the script's progress.

<dt><tt>wait <i>duration</i></tt> <dd>delays for <tt><i>duration</i></tt>.

<dt><tt>mark <i>name</i></tt> <dd>notes the time at which the command was executed.

<dt><tt>wait_from_mark <i>name</i> <i>duration</i></tt> <dd>delays until <tt><i>duration</i></tt> after the <tt><i>name</i></tt>d mark. It is an error if the indicated time has already passed. The mark can be re-used.

<dt><tt>go</tt> <dd>start executing the script.

</dl>

## Building

This is an [Alire](https://alire.ada.dev/docs/#introduction) crate; to build, say `alr build`.
