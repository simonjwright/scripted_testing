separate (Test)
package body First is

   type First is new Scripted_Testing.Command with null record;
   overriding
   function Tcl_Command
     (C      : access First;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type First_Event is new Scripted_Testing.Event with null record;
   overriding
   procedure Execute (E : First_Event);

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
         Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   procedure Execute (E : First_Event)
   is
   begin
      Put_Line ("first called at " & E.Source_Line);
   end Execute;

   The_First_Command : aliased First;

   procedure Initialize is
   begin
      Scripted_Testing.Register (The_Command => The_First_Command'Access,
                                 To_Be_Named => "first");
   end Initialize;

begin
   Initialize;
end First;
