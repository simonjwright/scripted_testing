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
   --  * 'out' (and 'inout') parameters can provided to be returned to
   --  the SUT,
   --
   --  * exceptions can be raised whn required,
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
   --  the flow of execution of the test ("go", "wait <t>",
   --  "finish", for example) are provided as standard. Other commands
   --  are to be provided to support the specific application to be
   --  tested: typically, "call_<procedure> <param1> <param2> ..."
   --  (where the parameters are those required by <procedure>) and
   --  "check_<procedure> <param1> <param2> ..." (to check the
   --  parameters passed on the last call to <procedure>).
   type Command is abstract tagged limited private;
   type Command_P is access all Command'Class;

   --  Called by Tcl to implement the command. Must return either
   --  Tcl.TCL_OK or Tcl.TCL_ERROR.
   function Tcl_Command
     (C      : access Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
     is abstract;

   --  A Command is to be registered with the Tcl interpreter.
   procedure Register (The_Command : Command_P;
                       To_Be_Named : String);

   --  The Commands that are called by a test script create matching
   --  Events which are posted to an internal event queue; the 'go'
   --  command (which should be the last in the test script), instead
   --  of posting an event, starts the queue.
   --
   --  The initial set of events would normally correspond to setup,
   --  for example arranging for stubbed lower-level interface calls
   --  to return appropriate values.
   --
   --  Once setup is complete, the software under test can start; how
   --  this is done depends on the framework in use. For example, with
   --  ColdFrame, the Dispatcher would be started.
   type Event is abstract tagged private;

   --  Events are picked off the queue and Execute is called for each,
   --  until either the end of the queue is reached (which would
   --  indicate that the test script succeeded) or execution raises
   --  an exception.
   procedure Execute (E : Event) is abstract;

   --  Used to report a script failure (for example, attempting to
   --  wait for a mark that hasn't been set). Any exception message is
   --  reported.
   Execution_Failure : exception;

   --  The scriptfile:line at which the event was created.
   function Source_Line (E : Event) return String;

   --  Used by Commands to post their corresponding Event.
   procedure Post (The_Event : Event'Class;
                   Interp    : Tcl.Tcl_Interp);

   --  Begin Tcl processing (and read the test script). Doesn't return
   --  (so all Commands must have been Registered before Start is
   --  called).
   procedure Start;

private

   type Command is abstract tagged limited null record;

   type Event is abstract tagged record
      Source : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Scripted_Testing;
