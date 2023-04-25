separate (Test)
package body Except is

   type Except is new Scripted_Testing.Command with null record;
   overriding
   function Tcl_Command
     (C      : not null access Except;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Except_Action is new Scripted_Testing.Action with record
      Str : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   overriding
   procedure Execute (A : Except_Action);

   overriding function Tcl_Command
     (C      : not null access Except;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      Str : Ada.Strings.Unbounded.Unbounded_String;
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type Interfaces.C.int;
   begin
      Put_Line ("'except' called from Ada.");
      for J in 0 .. Argc - 1 loop
         Str := Str & CArgv.Arg (Argv, J);
         if J < Argc - 1 then
            Str := Str & " ";
         end if;
      end loop;
      Scripted_Testing.Post
        (Except_Action'(Scripted_Testing.Action with
                        Str => Str),
         Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   overriding procedure Execute (A : Except_Action)
   is
   begin
      Put_Line ("except called at " & A.Source_Line);
      raise Constraint_Error with +A.Str;
   end Execute;

   The_Except_Command : aliased Except;

   procedure Initialize is
   begin
      Scripted_Testing.Register (The_Command => The_Except_Command'Access,
                                 To_Be_Named => "except");
   end Initialize;

begin
   Initialize;
end Except;
