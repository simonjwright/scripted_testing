with Scripted_Testing.Stubs.Scripting;
with Scripted_Testing.Callbacks;

with GNAT.String_Split;

package body Digital_IO.Scripting is

   package Set_Boolean
   is new Scripted_Testing.Stubs.Scripting.Set_Returned_Value
     (Returned_Type      => Boolean,
      Returned_Type_Name => "boolean",
      Value              => Boolean'Value);

   package Check_Boolean
   is new Scripted_Testing.Stubs.Scripting.Check_Passed_Value
     (Checked_Type      => Boolean,
      Checked_Type_Name => "boolean",
      Value             => Boolean'Value,
      Image             => Boolean'Image);

   package Check_Output_Signal
     is new Scripted_Testing.Stubs.Scripting.Check_Passed_Value
       (Checked_Type      => Output_Signal,
        Checked_Type_Name => "digital_io.output_signal",
        Value             => Output_Signal'Value,
        Image             => Output_Signal'Image);

   package Check_Boolean_For_Output_Signal
     is new Scripted_Testing.Stubs.Scripting.Check_Keyed_Value
       (Checked_Type      => Boolean,
        Checked_Type_Name => "boolean",
        Checked_Value     => Boolean'Value,
        Checked_Image     => Boolean'Image,
        Key_Type          => Output_Signal,
        Key_Type_Name     => "digital_io.output_signal",
        Key_Value         => Output_Signal'Value,
        Key_Image         => Output_Signal'Image);

   function Input_Signal_State_Value (S : String)
                                     return Digital_IO.Input_Signal_State;

   package Input_Signal_State_Callbacks
   is new Scripted_Testing.Callbacks
     (T                  => Digital_IO.Input_Signal_State,
      Callback_Type_Name => "digital_io.input_signal_state",
      Value              => Input_Signal_State_Value,
      Call_Callbacks     =>
        Digital_IO.Input_Signal_State_Callbacks.Call_Callbacks);

   function Input_Signal_State_Value (S : String)
                                     return Digital_IO.Input_Signal_State
   is
      Tokens : GNAT.String_Split.Slice_Set;
   begin
      GNAT.String_Split.Create (S          => Tokens,
                                From       => S,
                                Separators => " " & ASCII.HT,
                                Mode       => GNAT.String_Split.Multiple);
      if Natural (GNAT.String_Split.Slice_Count (Tokens)) /= 2 then
         raise Constraint_Error
           with "digital_io.input_signal_state requires 2 components";
      end if;
      return Result : Input_Signal_State do
         Result.S :=
           Input_Signal'Value (GNAT.String_Split.Slice
                                 (Tokens,
                                  GNAT.String_Split.Slice_Number (1)));
         Result.State :=
           Boolean'Value (GNAT.String_Split.Slice
                            (Tokens,
                             GNAT.String_Split.Slice_Number (2)));
      end return;
   end Input_Signal_State_Value;

end Digital_IO.Scripting;
