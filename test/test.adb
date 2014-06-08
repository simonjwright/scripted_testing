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

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with CArgv;
with Interfaces.C;
with Scripted_Testing;
with Tcl.Ada;

package body Test is

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   --  first

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

   --  except

   type Except is new Scripted_Testing.Command with null record;
   overriding
   function Tcl_Command
     (C      : access Except;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Except_Event is new Scripted_Testing.Event with record
      Str : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   overriding
   procedure Execute (E : Except_Event);

   function Tcl_Command
     (C      : access Except;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
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
        (Except_Event'(Scripted_Testing.Event with
                       Str => Str),
         Interp => Interp);
      return Tcl.TCL_OK;
   end Tcl_Command;

   procedure Execute (E : Except_Event)
   is
   begin
      Put_Line ("except called at " & E.Source_Line);
      raise Constraint_Error with +E.Str;
   end Execute;

   The_Except_Command : aliased Except;

begin
   Scripted_Testing.Register (The_Command => The_First_Command'Access,
                              To_Be_Named => "first");
   Scripted_Testing.Register (The_Command => The_Except_Command'Access,
                              To_Be_Named => "except");
end Test;
