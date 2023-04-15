--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with CArgv;
with Interfaces.C;
with Tcl.Ada;

package body Scripted_Testing.Callbacks is

   type Callback_Command
     is new Scripted_Testing.Command with null record;

   Callback_Name : constant String
     := "callback-"
       & Ada.Strings.Fixed.Translate
         (Callback_Type_Name,
          Ada.Strings.Maps.Constants.Lower_Case_Map);

   overriding
   function Tcl_Command
     (C      : not null access Callback_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

   type Callback_Action
     is new Scripted_Testing.Action with record
        Data : T;
     end record;

   overriding
   procedure Execute (A : Callback_Action);

   function Tcl_Command
     (C      : not null access Callback_Command;
      Interp : not null        Tcl.Tcl_Interp;
      Argc   :                 Interfaces.C.int;
      Argv   :                 CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (C);
      use type Interfaces.C.int;
   begin
      --  The argument is <value>
      if Argc /= 2 then
         Tcl.Ada.Tcl_AddErrorInfo
           (Interp,
            Callback_Name & " requires 1 argument");
         return Tcl.TCL_ERROR;
      end if;
      Scripted_Testing.Post
        (Callback_Action'(Scripted_Testing.Action with
                          Data => Value (CArgv.Arg (Argv, 1))),
         Interp => Interp);
      return Tcl.TCL_OK;
   exception
      when E : others =>
         Tcl.Ada.Tcl_AddErrorInfo
           (Interp,
            Callback_Name & ": " & Ada.Exceptions.Exception_Message (E));
         return Tcl.TCL_ERROR;
   end Tcl_Command;

   procedure Execute (A : Callback_Action)
   is
   begin
      Application_Callbacks.Call_Callbacks (A.Data);
   exception
      when Ex : others =>
         raise Scripted_Testing.Execution_Failure with
           Ada.Exceptions.Exception_Message (Ex);
   end Execute;

   The_Callback_Command :
     aliased Callback_Command;

   procedure Initialize is
   begin
      Scripted_Testing.Register
        (The_Command => The_Callback_Command'Access,
         To_Be_Named => Callback_Name);
   end Initialize;

begin
   Initialize;
end Scripted_Testing.Callbacks;
