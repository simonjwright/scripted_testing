--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: CC0-1.0

with Ada.Containers.Vectors;

package body Sample_Callbacks is

   package Callback_Vectors
   is new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Callback);

   Callbacks : Callback_Vectors.Vector;

   function Is_Registered (Proc : Callback) return Boolean
   is (for some P of Callbacks => Proc = P);

   procedure Call_Callbacks (With_Param : T) is
   begin
      for Proc of Callbacks loop
         Proc (With_Param);
      end loop;
   end Call_Callbacks;

   procedure Clear is
   begin
      Callbacks.Clear;
   end Clear;

   procedure Deregister (Proc : Callback) is
   begin
      for J in reverse Callbacks.First_Index .. Callbacks.Last_Index loop
         if Callbacks.Element (J) = Proc then
            Callbacks.Delete (J);
         end if;
      end loop;
   end Deregister;

   procedure Register (Proc : Callback) is
   begin
      Callbacks.Append (Proc);
   end Register;

end Sample_Callbacks;
