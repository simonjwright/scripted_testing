--  Copyright (C) 2014-23 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later

--  This package was originally developed as part of ColdFrame.

package Scripted_Testing.Stubs.Scripting is

   --  Provides Tcl commands related to Scripted_Testing.Stubs.
   --
   --  All names are case-insensitive.

   -----------------------
   --  S t a n d a r d  --
   -----------------------

   --  check_number_of_calls <subprogram-name> <natural>
   --
   --  At execution time, checks the number of calls to the
   --  subprogram.

   --  save_number_of_calls <subprogram-name>
   --
   --  At execution time, saves the current number of calls.

   --  check_number_of_new_calls <subprogram-name> <natural>
   --
   --  At execution time, checks the number of calls to the
   --  subprogram since the last save_number_of_calls (if any).

   ---------------------------------
   --  T y p e - s p e c i f i c  --
   ---------------------------------

   --  These generics support creating type-specific commands.
   --
   --  Note, values of types are always passed as one argument; if the
   --  type has more than one component, it is to be passed as a list,
   --  {component-1 component-2 ...}.

   generic
      type Returned_Type is private;
      Returned_Type_Name : String;
      with function Value (S : String) return Returned_Type is <>;
   package Set_Returned_Value is
      --  Creates a Tcl command set-<returned_type_name> (lowercased)
      --  which takes 3 or 4 arguments:
      --
      --  fully-qualified name of subprogram
      --  name of in-out or out parameter ("return" for a function
      --  return)
      --  the required value
      --  optionally, the call from which the value is to be returned
      --  (default is 0, implies "from now on").
      --
      --  At execution time, the created event sets the value to be
      --  returned by the stub.
   private
      procedure Initialize;
      --  Placed here to allow the package to have a body.
   end Set_Returned_Value;

   generic
      type Checked_Type is private;
      Checked_Type_Name : String;
      with function "=" (L, R : Checked_Type) return Boolean is <>;
      with function Value (S : String) return Checked_Type is <>;
      with function Image (V : Checked_Type) return String is <>;
   package Check_Passed_Value is
      --  Creates a Tcl command check-<checked_type_name> (lowercased)
      --  which takes 3 or 4 arguments:
      --
      --  fully-qualified name of subprogram
      --  name of in or in-out parameter
      --  expected value
      --  optionally, the call to check (default => latest, -1 =>
      --  last-but-one, 1 => first).
      --
      --  At execution time, the created event retrieves the actual
      --  value passed in the parameter and checks against the
      --  supplied expected value. If they aren't equal, it raises
      --  Execution_Failure with a suitable message.
   private
      procedure Initialize;
      --  Placed here to allow the package to have a body.
   end Check_Passed_Value;

   generic
      type Checked_Type is private;
      Checked_Type_Name : String;
      with function "=" (L, R : Checked_Type) return Boolean is <>;
      with function Checked_Value (S : String) return Checked_Type is <>;
      with function Checked_Image (V : Checked_Type) return String is <>;
      type Key_Type is private;
      Key_Type_Name : String;
      with function "=" (L, R : Key_Type) return Boolean is <>;
      with function Key_Value (S : String) return Key_Type is <>;
      with function Key_Image (V : Key_Type) return String is <>;
   package Check_Keyed_Value is
      --  Creates a Tcl command
      --  check-<checked_type_name>-for-<key_type_name> (all
      --  lowercased) which takes 5 arguments:
      --
      --  fully-qualified name of subprogram
      --  name of in or in-out key parameter
      --  key value
      --  name of in or in-out parameter to be checked
      --  expected value
      --
      --  At execution time, the created event finds the latest call
      --  at which the key parameter had the key value, retrieves the
      --  corresponding result parameter value and checks against the
      --  supplied expected value. If they aren't equal, it raises
      --  Execution_Failure with a suitable message.
   private
      procedure Initialize;
      --  Placed here to allow the package to have a body.
   end Check_Keyed_Value;

end Scripted_Testing.Stubs.Scripting;
