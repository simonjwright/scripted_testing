with Ada.Containers.Indefinite_Ordered_Maps;

with Tcl.Ada;

package body Scripted_Testing is

   --  Stores the registered commands. We only use a Map so as to be
   --  able to store the (indefinite) name and the associated Command.
   --
   --  Don't worry about deallocating the Commands at program exit.
   package Command_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String,
      Element_Type => Command_P);

   Commands : Command_Maps.Map;

   function Init (Interp : in Tcl.Tcl_Interp) return Interfaces.C.int;
   pragma Convention (C, Init);

   function Classwide_Tcl_Command
     (C      : Command_P;
      Interp : Tcl.Tcl_Interp;
      Argc   : Interfaces.C.int;
      Argv   : CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;
   pragma Convention (C, Classwide_Tcl_Command);

   procedure Register (The_Command : Command_P;
                       To_Be_Named : String)
   is
   begin
      Commands.Insert (Key => To_Be_Named, New_Item => The_Command);
   end Register;


   procedure Start
   is
      --  Argc and Argv include the command name
      Argc : Interfaces.C.int;
      Argv : CArgv.Chars_Ptr_Ptr;
   begin
      --  Get command-line arguments and put them into C-style "argv",
      --  as required by Tcl_Main.
      CArgv.Create (Argc, Argv);

      --  Start Tcl (and never return!)
      Tcl.Tcl_Main (Argc, Argv, Init'Access);
   end Start;


   function Init (Interp : in Tcl.Tcl_Interp) return Interfaces.C.int
   is
      procedure Create_Command (Position : Command_Maps.Cursor);
      procedure Create_Command (Position : Command_Maps.Cursor)
      is
         package Creator is new Tcl.Ada.Generic_Command (Command_P);
         Command : Tcl.Tcl_Command;
         pragma Unreferenced (Command);
      begin
         Command := Creator.Tcl_CreateCommand
           (Interp,
            Command_Maps.Key (Position),
            Classwide_Tcl_Command'Access,
            Command_Maps.Element (Position),
            null);
      end Create_Command;
      use type Interfaces.C.int;
   begin

      if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      Commands.Iterate (Create_Command'Access);

      --  Specify a user-specific startup file to invoke if the
      --  application is run interactively.  Typically the startup
      --  file is "~/.apprc" where "app" is the name of the
      --  application.  If this line is deleted then no user-specific
      --  startup file will be run under any conditions.

      --  N.B. will need to with Tcl.Ada if this is invoked.

      --  declare
      --     Result : constant String :=
      --        Tcl.Ada.Tcl_SetVar
      --          (Interp,
      --           "tcl_rcFileName",
      --           "~/.scripted_testingrc",
      --           Tcl.TCL_GLOBAL_ONLY);
      --     pragma Unreferenced (Result);  -- but wanted!
      --  begin
      --     return Tcl.TCL_OK;
      --  end;

      return Tcl.TCL_OK;

   end Init;


   function Classwide_Tcl_Command
     (C      : Command_P;
      Interp : Tcl.Tcl_Interp;
      Argc   : Interfaces.C.int;
      Argv   : CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
   begin
      return C.Tcl_Command (Interp, Argc, Argv);
   end Classwide_Tcl_Command;


end Scripted_Testing;
