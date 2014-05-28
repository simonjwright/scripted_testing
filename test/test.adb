with Ada.Text_IO; use Ada.Text_IO;
with CArgv;
with Interfaces.C;
with Scripted_Testing;
with Tcl.Ada;

package body Test is

   type First is new Scripted_Testing.Command with null record;
   overriding
   function Tcl_Command
     (C      : access First;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type First_Event is new Scripted_Testing.Event with null record;
   overriding
   function Execute (E : First_Event) return Scripted_Testing.Status;

   function Tcl_Command
     (C      : access First;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      use type Interfaces.C.int;
      pragma Unreferenced (C);
   begin
      Put_Line ("'first' called from Ada.");
      for J in 0 .. Argc - 1 loop
         Put_Line ("arg" & J'Img & " " & CArgv.Arg (Argv, J));
      end loop;
      Scripted_Testing.Post
        (First_Event'(Scripted_Testing.Event with
                      null record),
         From => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   function Execute (E : First_Event) return Scripted_Testing.Status
   is
   begin
      Put_Line ("first called at " & E.Source_Line);
      return Scripted_Testing.Success;
   end Execute;

   The_Command : aliased First;

begin
   Scripted_Testing.Register (The_Command => The_Command'Access,
                              To_Be_Named => "first");
end Test;
