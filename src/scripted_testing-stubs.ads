--  Copyright (C) 2005-23 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later

--  This package was originally developed as part of ColdFrame.

with Ada.Exceptions;
with Ada.Streams;
with Scripted_Testing.Utilities.Synchronization;

package Scripted_Testing.Stubs is

   --  Values are stored using Streams. For most uses, the space
   --  required for the streamed representation will be less than that
   --  for the in-store representation; however, for indefinite types,
   --  there is an additional overhead required to specify the actual
   --  object's constraints. This value (a number of bytes) should be
   --  enough for all but extreme cases.
   Storage_Overhead : constant := 128;

   --  Raised if a referenced subprogram isn't known.
   No_Subprogram : exception;

   --  Raised if a referenced parameter isn't known, or if an attempt
   --  is made to set the return value of a procedure.
   No_Parameter : exception;

   --  Raised if a required return or (in)out value is not found.
   No_Value : exception;

   --  Raised by Set_Output_Value or Set_Exception if a return or
   --  (in)out value or an exception has already been set and the
   --  'Override' parameter is False.
   Already_Set : exception;

   --  Raised by Set_Output_Value or Set_Exception if a return or
   --  (in)out value or an exception has not already been set and the
   --  'Override' parameter is True.
   Not_Already_Set : exception;

   -------------------------------
   --  T e s t   c o n t r o l  --
   -------------------------------

   --  Normally, you'd run each test scenario as a separate execution
   --  of the software under test in its scripting set up, in which
   --  case these two procedures needn't be called. They are for use
   --  if you run more than one scenario in the same execution.

   --  [Re]-initialize storage.
   procedure Set_Up;

   --  Free all storage.
   procedure Tear_Down;

   -------------------------------------------------------------
   --  O p e r a t i o n s   f o r   u s e r   s u p p o r t  --
   -------------------------------------------------------------

   --  Specify an output from a call to a stubbed operation for the
   --  type T.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram (eg, if dealing with procedure
   --  Domain.Class.Operation, "Domain.Class.Operation").
   --
   --  Normally the named parameter will be an "out" (perhaps "in
   --  out") parameter. The specified "To" value will be returned on
   --  the "For_Call"th call, and all subsequent calls until another
   --  "Set_Output_Value" call for the same parameter; if you want to
   --  have the first 4 calls to Domain.Class.Operation to set Output
   --  to 4, and any later ones to set it to 42, you'd say
   --
   --     Set_T ("Domain.Class.Operation", "Output", 4, 1);
   --     Set_T ("Domain.Class.Operation", "Output", 42, 5);
   --
   --  A special parameter name is "return". For "return", the To
   --  value will be the function result.
   --
   --  A previously stored value for a particular call can only be
   --  overridden if Override is True (when it must be).
   --
   --  Overhead_Bytes is the additional space reserved for the
   --  streamed representation.
   generic
      type T (<>) is private;
   procedure Set_Output_Value
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      To                   : T;
      For_Call             : Positive := 1;
      Override             : Boolean  := False;
      Overhead_Bytes       : Natural  := Storage_Overhead);

   --  Specify that a call to a stubbed operation is to raise an
   --  exception.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram (eg, if dealing with procedure
   --  Domain.Class.Operation, "Domain.Class.Operation").
   --
   --  Normally the exception will be raised for the specified call
   --  and all later calls; to stop this, use Ada.Exceptions.Null_Id.
   --
   --  A previously stored exception for a particular call can only be
   --  overridden if Override is True (when it must be).
   procedure Set_Exception
     (For_Subprogram_Named : String;
      E                    : Ada.Exceptions.Exception_Id;
      For_Call             : Positive := 1;
      Override             : Boolean  := False);

   --  Retrieve the number of calls made to the named subprogram.
   function Number_Of_Calls (For_Subprogram_Named : String) return Natural;

   --  Saves the count of calls made to the named subprogram.
   procedure Save_Number_Of_Calls (For_Subprogram_Named : String);

   --  Retrieve the number of calls made to the named subprogram since
   --  saved (if saved!)
   function Number_Of_New_Calls (For_Subprogram_Named : String) return Natural;

   --  Retrieve values passed to stubbed operations for the type T.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram (eg, if dealing with procedure
   --  Domain.Class.Operation, "Domain.Class.Operation").
   --
   --  The named parameter will be an "in" (perhaps "in out")
   --  parameter. To retrieve the value passed at the second call,
   --  you'd say
   --
   --     Result := Get_T
   --       ("Domain.Class.Operation", "Input", 2);
   --
   --  To retrieve the result of the last call, say
   --
   --     Result := Get_T
   --       ("Domain.Class.Operation", "Input", 0);
   --
   --  To retrieve the value passed at the last call but one, say
   --
   --     Result := Get_T
   --       ("Domain.Class.Operation", "Input", -1);
   generic
      type T (<>) is private;
   function Get_Input_Value
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      For_Call             : Integer := 0)
     return T;

   --  Retrieve values passed to stubbed operations in a parameter
   --  (For_Parameter_Named) of type Result_Type in the last call in
   --  which a value Had_Value was passed in another parameter
   --  (When_Parameter_Named) of type Key_Type.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram (eg, if dealing with procedure
   --  Domain.Class.Operation, "Domain.Class.Operation").
   --
   --  The named parameters will be "in" (perhaps "in out")
   --  parameters.
   --
   --  This facility is intended for the case where a value is passed
   --  with an index: for example,
   --
   --     Set_Colour (For_Lamp : Positive; To : Colour);
   --
   --  in which case the instantiation might be
   --
   --     function Get_Colour_For_Positive
   --       is new Get_Keyed_Input_Value (Positive, Colour);
   --
   --  and to retrieve the Colour to which Lamp 4 was last set you'd
   --  say
   --
   --   Result := Get_Colour_For_Positive
   --     ("Domain.Class.Set_Colour", "To", "For_Lamp", 4);
   generic
      type Key_Type (<>) is private;
      type Result_Type (<>) is private;
      with function "=" (L, R : Key_Type) return Boolean is <>;
   function Get_Keyed_Input_Value
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      When_Parameter_Named : String;
      Had_Value            : Key_Type)
     return Result_Type;

   -----------------------------------------------------------------
   --  O p e r a t i o n s   f o r   g e n e r a t e d   c o d e  --
   -----------------------------------------------------------------

   --  Must be called during elaboration to register the existence of
   --  a subprogram.
   procedure Register_Subprogram (Named : String);

   --  Must be called during elaboration to register the existence of
   --  a subprogram input parameter.
   procedure Register_Input_Parameter (Subprogram_Named : String;
                                       Parameter_Named  : String);

   --  Must be called during elaboration to register the existence of
   --  a subprogram output parameter (or function return).
   procedure Register_Output_Parameter (Subprogram_Named : String;
                                        Parameter_Named  : String);

   --  Local type for access to internal streams.
   type Stream_Access is access Ada.Streams.Root_Stream_Type'Class;

   --  Must be called at entry to a subprogram to prepare for
   --  recording and retrieving data for a call.
   --
   --  For_Subprogram_Named is the case-insensitive fully-qualified
   --  name of the subprogram.
   --
   --  Returns the call number (the number of times the subprogram has
   --  been called since the last Tear_Down, including this call).
   function Note_Entry (For_Subprogram_Named : String) return Positive;

   --  Called to get the stream into which the For_Call'th input value
   --  of For_Parameter_Named for the subprogram For_Subprogram_Named
   --  is to be stored, using type-name'Output.
   --
   --  Size_In_Bits is the size of the object to be streamed; 'Size
   --  will do.
   --
   --  Overhead_Bytes is the additional space reserved for the
   --  streamed representation.
   function Get_Input_Value_Stream
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      For_Call             : Positive;
      Size_In_Bits         : Natural;
      Overhead_Bytes       : Natural := Storage_Overhead)
     return Stream_Access;

   --  Called (after all input values have been saved) to check
   --  whether any exception is to be raised and, if so, raise it.
   --
   --  Exceptions are to be stored by using Set_Output_Value with an
   --  Exception_ID and the special parameter name "exception".
   procedure Check_For_Exception (For_Subprogram_Named : String;
                                  For_Call             : Positive);

   --  Called (after all input values have been saved and exceptions
   --  checked for) to get the stream from which the For_Call'th
   --  output value of For_Parameter_Named for the subprogram
   --  For_Subprogram_Named is to be retrieved, using type-name'Input.
   --
   --  For function return values, the values are to be stored by
   --  using Set_Output_Value with the special parameter name
   --  "return".
   function Get_Output_Value_Stream
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      For_Call             : Positive)
     return Stream_Access;

   --  Used for mutual exclusion via the RAII (Resource Acquisition Is
   --  Initialization) pattern.
   --
   --  declare
   --     L : Lock (Mutex'Access);
   --     pragma Unreferenced (L);
   --     --  now locked
   --  begin
   subtype Lock is Scripted_Testing.Utilities.Synchronization.Lock;
   Mutex : aliased Scripted_Testing.Utilities.Synchronization.Semaphore;

end Scripted_Testing.Stubs;
