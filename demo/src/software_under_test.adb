--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: CC0-1.0

with Digital_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with System;

package body Software_Under_Test is

   protected Simple_Callback_Handler is
      entry Get (Signal : out Digital_IO.Input_Signal_State);
      procedure Put (Signal : Digital_IO.Input_Signal_State);
   private
      Available : Boolean := False;
      Signal : Digital_IO.Input_Signal_State;
   end Simple_Callback_Handler;
   procedure Callback_Proc (Signal : Digital_IO.Input_Signal_State);
   --  Would've been good if the PO's Put had been acceptable!

   task Demo with Priority => System.Default_Priority + 1;

   protected body Simple_Callback_Handler is
      entry Get (Signal : out Digital_IO.Input_Signal_State)
        when Available is
      begin
         Available := False;
         Signal := Simple_Callback_Handler.Signal;
      end Get;
      procedure Put (Signal : Digital_IO.Input_Signal_State) is
      begin
         Simple_Callback_Handler.Signal := Signal;
         Available := True;
      end Put;
   end Simple_Callback_Handler;

   task body Demo is
      Signal : Digital_IO.Input_Signal_State;
   begin
      delay 0.5;
      loop
         Simple_Callback_Handler.Get (Signal);
         --  I know that input & output signals are usually differently
         --  named, but for the current purposes this will do.
         Digital_IO.Set (O => Signal.S, To_State => not Signal.State);
         delay 1.0;
      end loop;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Demo;

   procedure Callback_Proc (Signal : Digital_IO.Input_Signal_State) is
   begin
      Simple_Callback_Handler.Put (Signal);
   end Callback_Proc;

begin
   Digital_IO.Input_Signal_State_Callbacks.Register
     (Proc => Callback_Proc'Access);
end Software_Under_Test;
