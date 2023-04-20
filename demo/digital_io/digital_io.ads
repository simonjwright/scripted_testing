--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: CC0-1.0

with Sample_Callbacks;

package Digital_IO is

   subtype Input_Signal
     is Integer
     range 0 .. 15;
   --  Numbers the input (switch) signal pins.

   subtype Output_Signal
     is Integer
     range 0 .. 15;
   --  Numbers the output (lamp) signal pins.

   type Input_Signal_State is record

      S : Input_Signal;
      --  The input signal that has changed.

      State : Boolean;
      --  The state that the signal has changed to.

   end record;

   package Input_Signal_State_Callbacks
   is new Sample_Callbacks (Input_Signal_State);
   --  Provides application domains with callback facilities for input
   --  signal state changes.

   function Get
     (S : Input_Signal)
     return Boolean;
   --  Returns the current state of the input signal for pin S.

   procedure Set
     (O : Output_Signal;
      To_State : Boolean);
   --  Sets the given output signal to the given state.

end Digital_IO;
