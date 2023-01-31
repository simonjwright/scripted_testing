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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Ada.Text_IO;
with Tcl.Ada;

package body Scripted_Testing is

   ---------------------------------------
   --  S t r i n g   u t i l i t i e s  --
   ---------------------------------------

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   -------------------------------------------
   --  C o m m a n d   p r o c e s s i n g  --
   -------------------------------------------

   --  Stores the registered commands. We only use a Map so as to be
   --  able to store the (indefinite) name and the associated Command.
   --
   --  Don't worry about deallocating the Commands at program exit.
   package Command_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Command_P);

   Commands : Command_Maps.Map;

   function Init (Interp : Tcl.Tcl_Interp) return Interfaces.C.int;
   pragma Convention (C, Init);

   --  This is the Tcl_CmdProc that is invoked for all Commands. The
   --  Cmd parameter is, from the Tcl point of view, ClientData, which
   --  is an opaque pointer-sized object. From the Ada point of view,
   --  it's an access-to-classwide which is dispatched on to invoke
   --  the Ada Tcl_Command for the actual Command instance.
   function Classwide_Tcl_Command
     (Cmd    : Command_P;
      Interp : Tcl.Tcl_Interp;
      Argc   : Interfaces.C.int;
      Argv   : CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;
   pragma Convention (C, Classwide_Tcl_Command);

   procedure Register (The_Command : not null Command_P;
                       To_Be_Named :          String)
   is
      --  Note, this procedure doesn't actually register the command;
      --  that's done in Init.
   begin
      Commands.Insert (Key => To_Be_Named, New_Item => The_Command);
   end Register;

   procedure Start
   is
      Interp : constant Tcl.Tcl_Interp := Tcl.Tcl_CreateInterp;
      Status : Interfaces.C.int;

      use type Interfaces.C.int;
   begin
      if Ada.Command_Line.Argument_Count /= 1 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "must have one argument");
         return;
      end if;

      Status := Init (Interp);

      if Status /= Tcl.TCL_OK then
         raise Program_Error with "interpreter initialisation failed";
      end if;

      Status := Tcl.Ada.Tcl_EvalFile (Interp, Ada.Command_Line.Argument (1));

      if Status /= Tcl.TCL_OK then
         Ada.Text_IO.Put_Line (Tcl.Ada.Tcl_GetResult (Interp));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Start;

   function Init (Interp : Tcl.Tcl_Interp) return Interfaces.C.int
   is
      procedure Create_Command (Position : Command_Maps.Cursor);
      procedure Create_Command (Position : Command_Maps.Cursor)
      is
         --  This is a GNAT special; the warning is that Command_P
         --  doesn't correspond to a C type. But we know that the size
         --  has to be OK (it's checked).
         pragma Warnings (Off, "in instantiation at*");
         package Creator is new Tcl.Ada.Generic_Command (Command_P);
         pragma Warnings (On, "in instantiation at*");
         Command : Tcl.Tcl_Command;
         pragma Unreferenced (Command);
      begin
         Command := Creator.Tcl_CreateCommand
           (Interp,
            cmdName    => Command_Maps.Key (Position),
            proc       => Classwide_Tcl_Command'Access,
            data       => Command_Maps.Element (Position),
            deleteProc => null);
      end Create_Command;
      use type Interfaces.C.int;
   begin

      if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      --  Create the Tcl-side Commands corresponding to the registered
      --  Ada Tcl_Commands.
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
     (Cmd    : Command_P;
      Interp : Tcl.Tcl_Interp;
      Argc   : Interfaces.C.int;
      Argv   : CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
   begin
      return Cmd.Tcl_Command (Interp, Argc, Argv);
   end Classwide_Tcl_Command;

   ---------------------------------------
   --  E v e n t   p r o c e s s i n g  --
   ---------------------------------------

   package Action_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Action'Class);

   Queue : Action_Vectors.Vector;

   function Source_Line (A : Action) return String
   is
      Result : constant String := +A.Source;
   begin
      if Result'Length > 0 then
         return Result;
      else
         return "(unknown)";
      end if;
   end Source_Line;

   procedure Post (The_Action : Action'Class;
                   Interp     : not null Tcl.Tcl_Interp)
   is
      Copy : Action'Class := The_Action;
   begin
      Copy.Source := +Current_Source_Line (Interp);
      Queue.Append (Copy);
   end Post;

   -------------------------------
   --  T i m e   m a r k i n g  --
   -------------------------------

   --  Store data for 'mark', 'wait_from_mark'.
   package Mark_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Ada.Calendar.Time,
      "="          => Ada.Calendar."=");

   Marks : Mark_Maps.Map;

   -----------------------------------
   --  B a s i c   c o m m a n d s  --
   -----------------------------------

   --  echo  --

   type Echo_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : not null access Echo_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Echo_Action is new Action with record
      Text : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   procedure Execute (E : Echo_Action);

   overriding
   function Tcl_Command
     (C      : not null access Echo_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 2 then
         Tcl.Ada.Tcl_SetResult
           (Interp,
            Current_Source_Line (Interp)
              & ": "
              & "'echo' requires 1 argument");
         return Tcl.TCL_ERROR;
      end if;
      Post (Echo_Action'(Action
                         with Text => +CArgv.Arg (Argv, 1)),
            Interp => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   overriding procedure Execute (E : Echo_Action)
   is
   begin
      Ada.Text_IO.Put_Line ("echo: " & (+E.Text));
   end Execute;

   Echo : aliased Echo_Command;

   --  go  --

   type Go_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : not null access Go_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   overriding
   function Tcl_Command
     (C      : not null access Go_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C, Argv);
      use type Interfaces.C.int;
      use type Ada.Containers.Count_Type;
   begin
      if Argc /= 1 then
         Tcl.Ada.Tcl_SetResult
           (Interp,
            Current_Source_Line (Interp)
              & ": "
              & "'go' requires zero arguments");
         return Tcl.TCL_ERROR;
      end if;

      loop
         exit when Queue.Length = 0;
         declare
            E : constant Action'Class := Queue.First_Element;
         begin
            Queue.Delete_First;
            E.Execute;
         exception
            --  On error, we clear the extant actions and marks, not
            --   because a normal script would need this (we recommend
            --   a single 'go' per script, which _should_ run to
            --   completion) but to make testing the basic commands
            --   easier.
            when EF : Execution_Failure =>
               Queue.Clear;
               Marks.Clear;
               Tcl.Ada.Tcl_SetResult
                 (Interp,
                  +E.Source & ": " & Ada.Exceptions.Exception_Message (EF));
               return Tcl.TCL_ERROR;
            when O : others =>
               Queue.Clear;
               Marks.Clear;
               Tcl.Ada.Tcl_SetResult
                 (Interp,
                  +E.Source & ": " & Ada.Exceptions.Exception_Information (O));
               return Tcl.TCL_ERROR;
         end;
      end loop;

      return Tcl.TCL_OK;
   end Tcl_Command;

   Go : aliased Go_Command;

   --  mark  --

   type Mark_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : not null access Mark_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Mark_Action is new Action with record
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   procedure Execute (A : Mark_Action);

   overriding
   function Tcl_Command
     (C      : not null access Mark_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 2 then
         Tcl.Ada.Tcl_SetResult
           (Interp,
            Current_Source_Line (Interp)
              & ": "
              & "'mark' requires 1 argument");
         return Tcl.TCL_ERROR;
      end if;
      Post (Mark_Action'(Action
                         with Name => +CArgv.Arg (Argv, 1)),
            Interp => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   overriding procedure Execute (A : Mark_Action)
   is
      Name : constant String := +A.Name;
   begin
      if Marks.Contains (Name) then
         Ada.Text_IO.Put_Line ("mark '" & Name & "' already set");
         Marks.Replace (Key => Name, New_Item => Ada.Calendar.Clock);
      else
         Marks.Insert (Key => Name, New_Item => Ada.Calendar.Clock);
      end if;
   end Execute;

   Mark : aliased Mark_Command;

   --  wait  --

   type Wait_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : not null access Wait_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Wait_Action is new Action with record
      Period : Duration;
   end record;

   overriding
   procedure Execute (A : Wait_Action);

   overriding
   function Tcl_Command
     (C      : not null access Wait_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 2 then
         Tcl.Ada.Tcl_SetResult
           (Interp,
            Current_Source_Line (Interp)
              & ": "
              & "'wait' requires 1 argument");
         return Tcl.TCL_ERROR;
      end if;
      Post (Wait_Action'(Action
                         with Period => Duration'Value (CArgv.Arg (Argv, 1))),
            Interp => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Tcl.Ada.Tcl_SetResult
           (Interp,
            Current_Source_Line (Interp)
              & ": "
              & Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   overriding procedure Execute (A : Wait_Action)
   is
   begin
      delay A.Period;
   end Execute;

   Wait : aliased Wait_Command;

   --  wait_from_mark  --

   type Wait_From_Mark_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : not null access Wait_From_Mark_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Wait_From_Mark_Action is new Action with record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Interval : Duration;
   end record;

   overriding
   procedure Execute (A : Wait_From_Mark_Action);

   overriding
   function Tcl_Command
     (C      : not null access Wait_From_Mark_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 3 then
         Tcl.Ada.Tcl_SetResult
           (Interp,
            Current_Source_Line (Interp)
              & ": "
              & "'wait_from_mark' requires 2 arguments");
         return Tcl.TCL_ERROR;
      end if;
      Post
        (Wait_From_Mark_Action'
           (Action with
            Name     => +(CArgv.Arg (Argv, 1)),
            Interval => Duration'Value (CArgv.Arg (Argv, 2))),
         Interp => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Tcl.Ada.Tcl_SetResult
           (Interp,
            Current_Source_Line (Interp)
              & ": "
              & Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   overriding procedure Execute (A : Wait_From_Mark_Action)
   is
      Name     : constant String  := +A.Name;
      Position : constant Mark_Maps.Cursor := Marks.Find (Name);
      use type Mark_Maps.Cursor;
   begin
      if Position = Mark_Maps.No_Element then
         raise Execution_Failure with "no mark '" & Name & "'";
      end if;
      declare
         use type Ada.Calendar.Time;
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         End_Of_Wait : constant Ada.Calendar.Time
           := Mark_Maps.Element (Position) + A.Interval;
      begin
         if End_Of_Wait < Now then
            raise Execution_Failure
              with ("""wait_from_mark "
                      & Name
                      & Duration'Image (A.Interval)
                      & """ passed"
                      & Duration'Image (Now - End_Of_Wait)
                      & " seconds ago");
         else
            delay until End_Of_Wait;
         end if;
      end;
   end Execute;

   Wait_From_Mark : aliased Wait_From_Mark_Command;

   -------------------------
   --  U t i l i t i e s  --
   -------------------------

   function Current_Source_Line (Interp : Tcl.Tcl_Interp) return String
   is
      --  There seems to be no way to invoke the 'info' command
      --  programmatically, so we evaluate the Tcl version.
      --
      --  The reason for the 'catch' is that 'info frame -1' fails if
      --  the command is executed from the Tcl command line, rather
      --  than a script, and the 'dict get $inf file' call finds no
      --  'file' key. We have to use 'info frame -3' because of the
      --  extra stack frames used by the 'if/catch' construct.
      --
      --  You might think this is overkill for a feature (invocation
      --  from the command line) that will hardly ever be used.
      Script : constant String :=
        "if {![catch {info frame -3} inf]} "
        & "{return ""[file tail [dict get $inf file]]:[dict get $inf line]""}";
      Source_Line_Status : constant Interfaces.C.int
        := Tcl.Ada.Tcl_Eval (Interp, Script);
      use type Interfaces.C.int;
   begin
      if Source_Line_Status = Tcl.TCL_RETURN then
         return Tcl.Ada.Tcl_GetStringResult (Interp);
      else
         return "(unknown)";
      end if;
   end Current_Source_Line;

begin

   Register (The_Command => Echo'Access,
             To_Be_Named => "echo");

   Register (The_Command => Go'Access,
             To_Be_Named => "go");

   Register (The_Command => Mark'Access,
             To_Be_Named => "mark");

   Register (The_Command => Wait'Access,
             To_Be_Named => "wait");

   Register (The_Command => Wait_From_Mark'Access,
             To_Be_Named => "wait_from_mark");

end Scripted_Testing;
