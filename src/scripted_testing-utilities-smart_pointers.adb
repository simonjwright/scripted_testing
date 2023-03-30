--  Copyright (C) 2003-23 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.0

--  This unit has been copied from the Booch Components, where it was
--  BC.Support.Smart_Pointers.

with Ada.Unchecked_Deallocation;

package body Scripted_Testing.Utilities.Smart_Pointers is

   function Create (Value : P) return Pointer is
   begin
      return Pointer'(Ada.Finalization.Controlled
                      with Rep => new Node'(Count => 1,
                                            Value => Value));
   end Create;

   function Value (Ptr : Pointer) return P is
   begin
      if Ptr.Rep = null then
         return null;
      else
         return Ptr.Rep.Value;
      end if;
   end Value;

   overriding procedure Adjust (Obj : in out Pointer) is
   begin
      if Obj.Rep /= null then
         Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
   end Adjust;

   procedure Delete is new Ada.Unchecked_Deallocation (T, P);
   procedure Delete is new Ada.Unchecked_Deallocation (Node, Ref);

   --  Finalize may be called more than once on the same object.
   --
   --  The first time it's called, we may set Tmp to a non-null value
   --  which designates the actual shared object and then proceed to
   --  decrement the count and, if no references remain, delete the
   --  used memory. But, in any case, *this* smart pointer no longer
   --  references the actual object, so another call to Finalize will
   --  have no effect.
   overriding procedure Finalize (Obj : in out Pointer) is
      Tmp : Ref := Obj.Rep;
   begin
      Obj.Rep := null;
      if Tmp /= null then
         Tmp.Count := Tmp.Count - 1;
         if Tmp.Count = 0 then
            Delete (Tmp.Value);
            Delete (Tmp);
         end if;
      end if;
   end Finalize;

end Scripted_Testing.Utilities.Smart_Pointers;
