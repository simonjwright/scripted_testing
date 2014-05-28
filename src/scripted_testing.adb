with Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
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


   ---------------------------------------
   --  E v e n t   p r o c e s s i n g  --
   ---------------------------------------

   package Event_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => Event'Class);

   Queue : Event_Vectors.Vector;

   function Source_Line (E : Event) return String
   is
   begin
      return +E.Source_Line;
   end Source_Line;


   procedure Post (The_Event : Event'Class;
                   From : Tcl.Tcl_Interp)
   is
      Copy : Event'Class := The_Event;
      Source_Line_Status : constant Interfaces.C.int
        := Tcl.Ada.Tcl_Eval
          (From,
           "set inf [info frame -1];"
             & " return ""[dict get $inf file]:[dict get $inf line]""");
      use type Interfaces.C.int;
   begin
      if Source_Line_Status = Tcl.TCL_RETURN then
         Copy.Source_Line := +Tcl.Ada.Tcl_GetStringResult (From);
      end if;
      Queue.Append (Copy);
   end Post;


   -------------------------------
   --  T i m e   m a r k i n g  --
   -------------------------------

   --  Store data for 'mark', 'wait_from_mark'.
   package Mark_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Ada.Calendar.Time,
      "=" => Ada.Calendar."=");

   Marks : Mark_Maps.Map;


   -----------------------------------
   --  B a s i c   c o m m a n d s  --
   -----------------------------------

   --  echo  --

   type Echo_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : access Echo_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Echo_Event is new Event with record
      Text : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   function Execute (E : Echo_Event) return Status;

   function Tcl_Command
     (C      : access Echo_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 2 then
         Put_Line (Standard_Error, "'echo' requires 1 argument");
         return Tcl.TCL_ERROR;
      end if;
      Post (Echo_Event'(Event
                        with Text => +CArgv.Arg (Argv, 1)),
            From => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Put (Standard_Error, Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   function Execute (E : Echo_Event) return Status
   is
   begin
      Put_Line (Standard_Error, "echo: " & (+E.Text));
      return Success;
   end Execute;

   Echo : aliased Echo_Command;


   --  go  --

   type Go_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : access Go_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   function Tcl_Command
     (C      : access Go_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C, Interp, Argv);
      use type Interfaces.C.int;
      use type Ada.Containers.Count_Type;
   begin
      if Argc /= 1 then
         Put_Line (Standard_Error, "'go' requires 1 argument");
         return Tcl.TCL_ERROR;
      end if;

      loop
         exit when Queue.Length = 0;
         declare
            E : constant Event'Class := Queue.First_Element;
         begin
            Queue.Delete_First;
            if E.Execute = Failure then
               return Tcl.TCL_ERROR;
            end if;
         end;
      end loop;

      return Tcl.TCL_OK;
   exception
      when E : others =>
         Put (Standard_Error, Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   Go : aliased Go_Command;


   --  mark  --

   type Mark_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : access Mark_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Mark_Event is new Event with record
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   function Execute (E : Mark_Event) return Status;

   function Tcl_Command
     (C      : access Mark_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 2 then
         Put_Line (Standard_Error, "'mark' requires 1 argument");
         return Tcl.TCL_ERROR;
      end if;
      Post (Mark_Event'(Event
                        with Name => +CArgv.Arg (Argv, 1)),
            From => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Put (Standard_Error, Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   function Execute (E : Mark_Event) return Status
   is
      Name : constant String := +E.Name;
   begin
      if Marks.Contains (Name) then
         Put_Line (Standard_Error, "mark '" & Name & "' already set");
         Marks.Replace (Key => Name, New_Item => Ada.Calendar.Clock);
      else
         Marks.Insert (Key => Name, New_Item => Ada.Calendar.Clock);
      end if;
      return Success;
   end Execute;

   Mark : aliased Mark_Command;


   --  wait  --

   type Wait_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : access Wait_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Wait_Event is new Event with record
      Period : Duration;
   end record;

   overriding
   function Execute (E : Wait_Event) return Status;

   function Tcl_Command
     (C      : access Wait_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 2 then
         Put_Line (Standard_Error, "'wait' requires 1 argument");
         return Tcl.TCL_ERROR;
      end if;
      Post (Wait_Event'(Event
                        with Period => Duration'Value (CArgv.Arg (Argv, 1))),
            From => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Put (Standard_Error, Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   function Execute (E : Wait_Event) return Status
   is
   begin
      delay E.Period;
      return Success;
   end Execute;

   Wait : aliased Wait_Command;


   --  wait_from_mark  --

   type Wait_From_Mark_Command is new Command with null record;

   overriding
   function Tcl_Command
     (C      : access Wait_From_Mark_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Wait_From_Mark_Event is new Event with record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Period : Duration;
   end record;

   overriding
   function Execute (E : Wait_From_Mark_Event) return Status;

   function Tcl_Command
     (C      : access Wait_From_Mark_Command;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      if Argc /= 3 then
         Put_Line (Standard_Error, "'wait_from_mark' requires 2 arguments");
         return Tcl.TCL_ERROR;
      end if;
      Post
        (Wait_From_Mark_Event'
           (Event with
            Name => +(CArgv.Arg (Argv, 1)),
            Period => Duration'Value (CArgv.Arg (Argv, 2))),
         From => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Put (Standard_Error, Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   function Execute (E : Wait_From_Mark_Event) return Status
   is
      Name     : constant String  := +E.Name;
      Position : Mark_Maps.Cursor := Marks.Find (Name);
      use type Mark_Maps.Cursor;
   begin
      if Position = Mark_Maps.No_Element then
         Put (Standard_Error, "no mark '" & Name & "'");
         return Failure;
      end if;
      declare
         use type Ada.Calendar.Time;
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         End_Of_Wait : constant Ada.Calendar.Time
           := Mark_Maps.Element (Position) + E.Period;
      begin
         Marks.Delete (Position => Position);
         if End_Of_Wait < Now then
            Put (Standard_Error,
                 "mark '"
                   & Name
                   & "'  at "
                   & Source_Line (E)
                   & " passed"
                   & Duration'Image (Now - End_Of_Wait)
                   & " seconds ago");
            return Failure;
         else
            delay until End_Of_Wait;
            return Success;
         end if;
      end;
   end Execute;

   Wait_From_Mark : aliased Wait_From_Mark_Command;


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
