--  Copyright (C) 2003-23 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.0

--  This unit has been copied from the Booch Components, where it was
--  BC.Support.Smart_Pointers.

private with Ada.Finalization;

generic
   type T (<>) is limited private;
   type P is access T;
package Scripted_Testing.Utilities.Smart_Pointers is

   --  pragma Preelaborate;

   type Pointer is private;
   --  A Pointer variable encapsulates a reference-counted accessor of
   --  type P (to a T).

   Null_Pointer : constant Pointer;
   --  Assign this to a Pointer when you've finished with its contents.

   function Create (Value : P) return Pointer;
   pragma Inline (Create);
   --  Returns a new encapsulation. You must NOT deallocate the Value
   --  passed; it will be deallocated when there are no more
   --  references to it.

   function Value (Ptr : Pointer) return P;
   pragma Inline (Value);
   --  returns the encapsulated pointer.

private

   type Node is record
      Count : Natural := 0;
      Value : P;
   end record;
   type Ref is access Node;

   type Pointer is new Ada.Finalization.Controlled with record
      Rep : Ref;
   end record;

   overriding procedure Adjust (Obj : in out Pointer);
   overriding procedure Finalize (Obj : in out Pointer);

   Null_Pointer : constant Pointer
     := Pointer'(Ada.Finalization.Controlled with Rep => null);

end Scripted_Testing.Utilities.Smart_Pointers;
