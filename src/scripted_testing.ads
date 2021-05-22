--  Copyright Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  This package is distributed
--  in the hope that it will be useful, but WITHOUT ANY WARRANTY;
--  without even the implied warranty of MERCHANTABILITY or FITNESS
--  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
--  more details.  You should have received a copy of the GNU General
--  Public License distributed with this package; see file COPYING3.
--  If not, go to http://www.gnu.org/licenses for a complete copy of
--  the license.

with Interfaces.C;
with CArgv;
with Tcl;

private with Ada.Strings.Unbounded;

package Scripted_Testing is

   --  To set the scene, the software under test (SUT) forms part of a
   --  system. Generally, other parts of the system are higher-level
   --  (which can call interface subprograms presented by the SUT) or
   --  lower-level (whose interfaces can be called by the SUT). It's
   --  assumed that the lower-level, called, interface subprograms
   --  are stubbed so that:
   --
   --  * 'in' and 'in out' parameters can be recorded for later
   --  checking,
   --
   --  * 'out' (and 'in out') parameters can provided to be returned
   --  to the SUT,
   --
   --  * exceptions can be raised when required,
   --
   --  * the number of calls to the subprogram can be checked.
   --
   --  The stubbing facilities of ColdFrame meet the above
   --  requirements.
   --
   --  The scripting language supported is Tcl. The reason for
   --  choosing Tcl rather than Python or Lua is that Tcl's interface
   --  is entirely string-based; this is important, considering the
   --  need to specifiy values of enumerated types.

   --  A Command corresponds to a script command. Commands to control
   --  the flow of execution of the test are provided as
   --  standard. Other commands are to be provided to support the
   --  specific application to be tested: typically, "call_<procedure>
   --  <param1> <param2> ..."  (where the parameters are those
   --  required by <procedure>) and "check_<subprogram> <parameter>
   --  <value>" (to check the value passed to <procedure> in
   --  <parameter> on the last call).
   --
   --  The standard commands are
   --
   --  echo "string": outputs "string" to the terminal at run
   --  time. Useful to mark the script's progress.
   --
   --  wait <duration>: delays for <duration>.
   --
   --  mark <name>: notes the time at which the command was executed.
   --
   --  wait_from_mark <name> <duration>: delays until <duration> after
   --  the <name>d mark. It is an error if the indicated time has
   --  already passed. The mark can be re-used.
   --
   --  go: start executing the script (see below).
   type Command is abstract tagged limited private;
   type Command_P is access all Command'Class;
   pragma Convention (C, Command_P);

   --  Called by Tcl to implement the command. Must return either
   --  Tcl.TCL_OK or Tcl.TCL_ERROR.
   not overriding
   function Tcl_Command
     (C      : not null access Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
     is abstract;

   --  Can be used if it's necessary for a Tcl_Command to report
   --  errors in the input.
   function Current_Source_Line (Interp : in Tcl.Tcl_Interp) return String;

   --  A Command is to be registered with the Tcl interpreter.
   procedure Register (The_Command : not null Command_P;
                       To_Be_Named :          String);

   --  The Commands that are called by a test script create matching
   --  Actions which are posted to an internal action queue; the 'go'
   --  command (which should be the last in the test script), instead
   --  of posting an action, starts the queue.
   --
   --  The initial set of actions would normally correspond to setup,
   --  for example arranging for stubbed lower-level interface calls
   --  to return appropriate values.
   --
   --  Once setup is complete, the software under test can start; how
   --  this is done depends on the framework in use. For example, with
   --  ColdFrame, the Dispatcher would be started.

   type Action is abstract tagged private;

   --  Actions are picked off the queue and Execute is called for each,
   --  until either the end of the queue is reached (which would
   --  indicate that the test script succeeded) or execution raises
   --  an exception.
   not overriding
   procedure Execute (A : Action) is abstract;

   --  Used to report a script failure (for example, attempting to
   --  wait for a mark that hasn't been set). Any exception message is
   --  reported.
   Execution_Failure : exception;

   --  The scriptfile:line at which the action was created.
   not overriding
   function Source_Line (A : Action) return String;

   --  Used by Commands to post their corresponding Action.
   procedure Post (The_Action : Action'Class;
                   Interp     : not null Tcl.Tcl_Interp);

   --  Begin Tcl processing (and read the test script). Doesn't return
   --  (so all Commands must have been Registered before Start is
   --  called).
   procedure Start;

private

   type Command is abstract tagged limited null record;

   type Action is abstract tagged record
      Source : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Scripted_Testing;
