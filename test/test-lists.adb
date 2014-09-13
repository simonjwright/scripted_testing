separate (Test)
package body Lists is

   type Lists is new Scripted_Testing.Command with null record;
   overriding
   function Tcl_Command
     (C      : not null access Lists;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Lists_Event is new Scripted_Testing.Event with null record;
   overriding
   procedure Execute (E : Lists_Event);

   function Tcl_Command
     (C      : not null access Lists;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      Put_Line ("'lists' called from Ada.");
      for J in 0 .. Argc - 1 loop
         Put_Line ("arg" & J'Img & " " & CArgv.Arg (Argv, J));
         declare
            C : aliased Interfaces.C.int;
            A : aliased CArgv.Chars_Ptr_Ptr;
            procedure Tcl_Free (Ptr : CArgv.Chars_Ptr_Ptr);
            pragma Import (C, Tcl_Free, "Tcl_Free");
         begin
            if Tcl.Ada.Tcl_SplitList (Interp,
                                      Cargv.Arg (Argv, J),
                                      C'Access,
                                      A'Access) /= Tcl.TCL_OK then
               return Tcl.TCL_ERROR;
            else
               for K in 0 .. C - 1 loop
                  Put_Line (J'Img & " " & K'Img & " " & Cargv.Arg (A, K));
               end loop;
               Tcl_Free (A);
            end if;
         end;
      end loop;
      --  Scripted_Testing.Post
      --    (Lists_Event'(Scripted_Testing.Event with
      --                   Str => Str),
      --     Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   procedure Execute (E : Lists_Event)
   is
   begin
      Put_Line ("lists called at " & E.Source_Line);
   end Execute;

   The_Lists_Command : aliased Lists;

   procedure Initialize is
   begin
      Scripted_Testing.Register (The_Command => The_Lists_Command'Access,
                                 To_Be_Named => "lists");
   end Initialize;

begin
   Initialize;
end Lists;
