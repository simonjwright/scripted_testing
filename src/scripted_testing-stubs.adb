--  Copyright (C) 2005-23 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later

--  This package was originally developed as part of ColdFrame.

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

with Scripted_Testing.Utilities.Memory_Streams;
with Scripted_Testing.Utilities.Smart_Pointers;

package body Scripted_Testing.Stubs is

   -------------------------------------
   --  D a t a   s t r u c t u r e s  --
   -------------------------------------

   --  We need to count subprogram entries ..

   package String_Bags is

      type Element is record
         Count : Natural := 0;  -- To ensure correct initialization
      end record;

      package String_To_Natural_Maps
        is new Ada.Containers.Indefinite_Hashed_Maps
          (Element_Type    => Element,
           Key_Type        => String,
           Hash            => Ada.Strings.Hash_Case_Insensitive,
           Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

      type Bag is new String_To_Natural_Maps.Map with null record;
      --  Using a null record extension means we don't need to
      --  override parent functions-returning-parent-type.

      not overriding procedure Add
        (B : in out Bag;
         S :        String);

      not overriding function Count
        (B : Bag;
         S : String) return Natural;

      not overriding procedure Reset
        (B : in out Bag;
         S :        String);

   end String_Bags;

   --  .. and saved counts.
   package String_To_Count_Maps
     is new Ada.Containers.Indefinite_Hashed_Maps
       (Element_Type    => Natural,
        Key_Type        => String,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   --  We need to check for the existence of subprograms and
   --  parameters/results.
   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => String,
      Hash                => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Elements => Ada.Strings.Equal_Case_Insensitive,
      "="                 => Ada.Strings.Equal_Case_Insensitive);

   --  We need Memory Streams to hold values, so we need pointers
   --  because Streams are limited. This also covers the need for
   --  varying-capacity Memory Streams. Use smart pointers so that the
   --  Streams get automatically freed when the Container they're in
   --  gets cleared.
   package Stream_Pointers
   is new Utilities.Smart_Pointers (T => Ada.Streams.Root_Stream_Type'Class,
                                    P => Stream_Access);

   --  For Inputs, we need plain collections of Memory Streams (there
   --  will be an input for each call).
   package Stream_Pointer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Stream_Pointers.Pointer,
      "="          => Stream_Pointers."=");

   --  Support for retrieving values from vectors of streams
   generic
      type T (<>) is private;
   function Streamed_Value (From     : Stream_Pointer_Vectors.Vector;
                            At_Index : Positive) return T;

   --  We need to map from subprogram.parameter names to collections
   --  of Memory Streams.
   package Input_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type    => Stream_Pointer_Vectors.Vector,
      Key_Type        => String,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
      "="             => Stream_Pointer_Vectors."=");

   --  For Exceptions, we need sparse arrays of Exception_Ids. We can
   --  find the appropriate entry for a particular Ordinal by reverse
   --  iterating until we come to an entry whose Ordinal is less than
   --  or equal to the one we require (or, of course, the beginning of
   --  the array).
   --
   --  We use a map ordered on Positive to emulate a sorted vector,
   --  which isn't provided by Ada.Containers.
   package Sparse_Exceptions is new Ada.Containers.Ordered_Maps
     (Key_Type     => Positive,
      Element_Type => Ada.Exceptions.Exception_Id,
      "="          => Ada.Exceptions."=");

   package Sparse_Exception_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Sparse_Exceptions.Map,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
      "="             => Sparse_Exceptions."=");

   --  For Outputs, we need sparse arrays of Memory Streams. We can
   --  find the appropriate entry for a particular Ordinal by reverse
   --  iterating until we come to an entry whose Ordinal is less than
   --  or equal to the one we require (or, of course, the beginning of
   --  the array).

   --  We hold two Memory Streams; Stream is for the original data,
   --  Copy is refilled each time the data is to be read (so we don't
   --  get End_Error if it's read more than once).
   type Output_Cell is record
      Stream : Stream_Pointers.Pointer;
      Copy   : Stream_Pointers.Pointer;
   end record;

   --  We use a map ordered on Positive to emulate a sorted vector,
   --  which isn't provided by Ada.Containers.
   package Sparse_Outputs is new Ada.Containers.Ordered_Maps
     (Key_Type     => Positive,
      Element_Type => Output_Cell);

   package Sparse_Output_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type    => Sparse_Outputs.Map,
      Key_Type        => String,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
      "="             => Sparse_Outputs."=");

   --  Validity checking information, set up during elaboration of
   --  generated code.
   Subprograms : String_Sets.Set;
   Input_Parameters : String_Sets.Set;
   Output_Parameters : String_Sets.Set;

   --  Test storage, managed by Set_Up and Tear_Down.
   Entries : String_Bags.Bag;
   Saved_Entries : String_To_Count_Maps.Map;
   Inputs : Input_Maps.Map;
   Exceptions : Sparse_Exception_Maps.Map;
   Outputs : Sparse_Output_Maps.Map;

   -------------------------------
   --  T e s t   c o n t r o l  --
   -------------------------------

   procedure Set_Up is
   begin
      Entries.Clear;
      Saved_Entries.Clear;
      Inputs.Clear;
      Exceptions.Clear;
      Outputs.Clear;
   end Set_Up;

   procedure Tear_Down is
   begin
      Entries.Clear;
      Saved_Entries.Clear;
      Inputs.Clear;
      Exceptions.Clear;
      Outputs.Clear;
   end Tear_Down;

   -------------------------------------------------------------
   --  O p e r a t i o n s   f o r   u s e r   s u p p o r t  --
   -------------------------------------------------------------

   procedure Set_Output_Value
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      To                   : T;
      For_Call             : Positive := 1;
      Override             : Boolean  := False;
      Overhead_Bytes       : Natural  := Storage_Overhead) is
      SP : constant String := For_Subprogram_Named & "." & For_Parameter_Named;
      subtype SEO is Ada.Streams.Stream_Element_Offset;
      Size : constant SEO := SEO ((To'Size + 7) / 8 + Overhead_Bytes);
      Lck : Lock (Mutex'Access);
      pragma Unreferenced (Lck);
   begin
      if not Subprograms.Contains (For_Subprogram_Named) then
         raise No_Subprogram
           with "subprogram " & For_Subprogram_Named & " not known";
      end if;
      if not Output_Parameters.Contains (SP) then
         if Ada.Strings.Fixed.Translate
           (For_Parameter_Named,
            Ada.Strings.Maps.Constants.Lower_Case_Map)
           = "return"
         then
            raise No_Parameter
              with For_Subprogram_Named & " is not a function";
         else
            raise No_Parameter with "parameter " & SP & " not known";
         end if;
      end if;
      if not Outputs.Contains (SP) then
         declare
            Coll : Sparse_Outputs.Map;
         begin
            Outputs.Insert (SP, Coll);
         end;
      end if;
      declare
         SOM_Cursor : constant Sparse_Output_Maps.Cursor := Outputs.Find (SP);
         SO : Sparse_Outputs.Map := Sparse_Output_Maps.Element (SOM_Cursor);
         SO_Cursor : constant Sparse_Outputs.Cursor := SO.Find (For_Call);
         use type Sparse_Outputs.Cursor;
         New_Cell : constant Output_Cell :=
           (Stream => Stream_Pointers.Create
              (new Utilities.Memory_Streams.Stream_Type (Size)),
            Copy => Stream_Pointers.Create
              (new Utilities.Memory_Streams.Stream_Type (Size)));
      begin
         if SO_Cursor = Sparse_Outputs.No_Element then
            if Override then
               raise Not_Already_Set;
            else
               SO.Insert (For_Call, New_Cell);
            end if;
         else
            if not Override then
               raise Already_Set;
            else
               SO.Replace_Element (SO_Cursor, New_Cell);
            end if;
         end if;
         T'Output (Stream_Pointers.Value (New_Cell.Stream), To);
         Outputs.Replace_Element (SOM_Cursor, SO);
      end;
   end Set_Output_Value;

   procedure Set_Exception
     (For_Subprogram_Named : String;
      E                    : Ada.Exceptions.Exception_Id;
      For_Call             : Positive := 1;
      Override             : Boolean  := False) is
      Lck : Lock (Mutex'Access);
      pragma Unreferenced (Lck);
   begin
      if not Subprograms.Contains (For_Subprogram_Named) then
         raise No_Subprogram
           with "subprogram " & For_Subprogram_Named & " not known";
      end if;
      if not Exceptions.Contains (For_Subprogram_Named) then
         declare
            SE : Sparse_Exceptions.Map;
         begin
            SE.Insert (For_Call, E);
            Exceptions.Insert (For_Subprogram_Named, SE);
         end;
         return;
      end if;
      declare
         SEM_Cursor : constant Sparse_Exception_Maps.Cursor
           := Exceptions.Find (For_Subprogram_Named);
         SE : Sparse_Exceptions.Map
           := Sparse_Exception_Maps.Element (SEM_Cursor);
         SE_Cursor : constant Sparse_Exceptions.Cursor := SE.Find (For_Call);
         use type Sparse_Exceptions.Cursor;
      begin
         if SE_Cursor = Sparse_Exceptions.No_Element then
            if Override then
               raise Not_Already_Set;
            else
               SE.Insert (For_Call, E);
            end if;
         else
            if not Override then
               raise Already_Set;
            else
               SE.Replace_Element (SE_Cursor, E);
            end if;
         end if;
         Exceptions.Replace_Element (SEM_Cursor, SE);
      end;
   end Set_Exception;

   function Number_Of_Calls (For_Subprogram_Named : String) return Natural is
      Lck : Lock (Mutex'Access);
      pragma Unreferenced (Lck);
   begin
      if not Subprograms.Contains (For_Subprogram_Named) then
         raise No_Subprogram
           with "subprogram " & For_Subprogram_Named & " not known";
      end if;
      return Entries.Count (For_Subprogram_Named);
   end Number_Of_Calls;

   procedure Save_Number_Of_Calls (For_Subprogram_Named : String) is
      Lck : Lock (Mutex'Access);
      pragma Unreferenced (Lck);
   begin
      if not Subprograms.Contains (For_Subprogram_Named) then
         raise No_Subprogram
           with "subprogram " & For_Subprogram_Named & " not known";
      end if;
      declare
         Count_So_Far : constant Natural :=
           Entries.Count (For_Subprogram_Named);
         C : constant String_To_Count_Maps.Cursor :=
           Saved_Entries.Find (For_Subprogram_Named);
         use type String_To_Count_Maps.Cursor;
      begin
         if C = String_To_Count_Maps.No_Element then
            Saved_Entries.Insert (For_Subprogram_Named, Count_So_Far);
         else
            Saved_Entries.Replace_Element
              (Position => C,
               New_Item => Count_So_Far);
         end if;
      end;
   end Save_Number_Of_Calls;

   function Number_Of_New_Calls (For_Subprogram_Named : String) return Natural
   is
      Lck : Lock (Mutex'Access);
      pragma Unreferenced (Lck);
   begin
      if not Subprograms.Contains (For_Subprogram_Named) then
         raise No_Subprogram
           with "subprogram " & For_Subprogram_Named & " not known";
      end if;
      declare
         C : constant String_To_Count_Maps.Cursor :=
           Saved_Entries.Find (For_Subprogram_Named);
         use type String_To_Count_Maps.Cursor;
      begin
         if C = String_To_Count_Maps.No_Element then
            return Entries.Count (For_Subprogram_Named);
         else
            return Entries.Count (For_Subprogram_Named)
              - String_To_Count_Maps.Element (C);
         end if;
      end;
   end Number_Of_New_Calls;

   function Get_Input_Value
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      For_Call             : Integer := 0) return T is
      SP : constant String := For_Subprogram_Named & "." & For_Parameter_Named;
      Lck : Lock (Mutex'Access);
      pragma Unreferenced (Lck);
   begin
      if not Subprograms.Contains (For_Subprogram_Named) then
         raise No_Subprogram
           with "subprogram " & For_Subprogram_Named & " not known";
      end if;
      if not Input_Parameters.Contains (SP) then
         raise No_Parameter with "parameter " & SP & " not known";
      end if;
      if not Inputs.Contains (SP) then
         raise No_Value with "input " & SP & " not found";
      end if;
      declare
         Pointers : Stream_Pointer_Vectors.Vector renames Inputs.Element (SP);
         function Result is new Streamed_Value (T);
         Len : constant Natural := Natural (Pointers.Length);
      begin
         if Len = 0 then
            raise No_Value with For_Subprogram_Named & " never called";
         elsif For_Call = 0 then
            return Result (Pointers, Len);
         elsif For_Call > Len then
            raise No_Value
              with For_Subprogram_Named & " only called" & Len'Img & " times";
         elsif For_Call in 1 .. Len then
            return Result (Pointers, For_Call);
         elsif Len + For_Call in 1 .. Len  then
            --  For_Call must be negative.
            return Result (Pointers, Len + For_Call);
         else
            raise No_Value
              with For_Subprogram_Named & " only called" & Len'Img & " times";
         end if;
      end;
   end Get_Input_Value;

   function Get_Keyed_Input_Value
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      When_Parameter_Named : String;
      Had_Value            : Key_Type)
     return Result_Type
   is
      NP : constant String
        := For_Subprogram_Named & "." & When_Parameter_Named;
      SP : constant String
        := For_Subprogram_Named & "." & For_Parameter_Named;
      Lck : Lock (Mutex'Access);
      pragma Unreferenced (Lck);
   begin
      if not Subprograms.Contains (For_Subprogram_Named) then
         raise No_Subprogram
           with "subprogram " & For_Subprogram_Named & " not known";
      end if;
      if not Input_Parameters.Contains (NP) then
         raise No_Parameter with "parameter " & NP & " not known";
      end if;
      if not Inputs.Contains (NP) then
         raise No_Value with "input " & NP & " not found";
      end if;
      if not Input_Parameters.Contains (SP) then
         raise No_Parameter with "parameter " & SP & " not known";
      end if;
      if not Inputs.Contains (SP) then
         raise No_Value with "input " & SP & " not found";
         --  actually, I think there has to be such an input
      end if;
      declare
         function Get_Key is new Streamed_Value (Key_Type);
         function Get_Result is new Streamed_Value (Result_Type);
         NP_Pointers : Stream_Pointer_Vectors.Vector
           renames Inputs.Element (NP);
         SP_Pointers : Stream_Pointer_Vectors.Vector
           renames Inputs.Element (SP);
         Len : constant Natural := Natural (NP_Pointers.Length);
      begin
         if Len = 0 then
            raise No_Value with For_Subprogram_Named & " never called";
         end if;
         for J in reverse 1 .. Len loop
            if Get_Key (NP_Pointers, J) = Had_Value then
               return Get_Result (SP_Pointers, J);
            end if;
         end loop;
         raise No_Value with "no matching input for " & NP;
      end;
   end Get_Keyed_Input_Value;

   -----------------------------------------------------------------
   --  O p e r a t i o n s   f o r   g e n e r a t e d   c o d e  --
   -----------------------------------------------------------------

   procedure Register_Subprogram (Named : String) is
   begin
      Subprograms.Insert (Named);
   end Register_Subprogram;

   procedure Register_Input_Parameter (Subprogram_Named : String;
                                       Parameter_Named  : String) is
   begin
      Input_Parameters.Insert (Subprogram_Named & "." & Parameter_Named);
   end Register_Input_Parameter;

   procedure Register_Output_Parameter (Subprogram_Named : String;
                                        Parameter_Named  : String) is
   begin
      Output_Parameters.Insert (Subprogram_Named & "." & Parameter_Named);
   end Register_Output_Parameter;

   function Note_Entry (For_Subprogram_Named : String) return Positive is
   begin
      Entries.Add (For_Subprogram_Named);
      return Entries.Count (For_Subprogram_Named);
   end Note_Entry;

   function Get_Input_Value_Stream
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      For_Call             : Positive;
      Size_In_Bits         : Natural;
      Overhead_Bytes       : Natural := Storage_Overhead)
     return Stream_Access is
      subtype SEO is Ada.Streams.Stream_Element_Offset;
      SP : constant String := For_Subprogram_Named & "." & For_Parameter_Named;
      Size : constant SEO
        := SEO ((Size_In_Bits + 7) / 8 + Overhead_Bytes);
      Str : constant Stream_Pointers.Pointer
        := Stream_Pointers.Create
        (new Scripted_Testing.Utilities.Memory_Streams.Stream_Type (Size));
   begin
      if not Inputs.Contains (SP) then
         declare
            C : Stream_Pointer_Vectors.Vector;
         begin
            Inputs.Insert (SP, C);
         end;
      end if;
      declare
         Position : constant Input_Maps.Cursor := Inputs.Find (SP);
         Coll : Stream_Pointer_Vectors.Vector := Input_Maps.Element (Position);
      begin
         pragma Assert
           (For_Call = Natural (Coll.Length) + 1,
              "mismatch in number of calls");
         Coll.Append (Str);
         Inputs.Replace_Element (Position, Coll);
         return Stream_Pointers.Value (Str);
      end;
   end Get_Input_Value_Stream;

   procedure Check_For_Exception (For_Subprogram_Named : String;
                                  For_Call             : Positive) is
   begin
      if not Exceptions.Contains (For_Subprogram_Named) then
         return;
      end if;
      declare
         SE : constant Sparse_Exceptions.Map
           := Exceptions.Element (For_Subprogram_Named);
         SE_Cursor : Sparse_Exceptions.Cursor := SE.Last;
         use type Sparse_Exceptions.Cursor;
      begin
         while SE_Cursor /= Sparse_Exceptions.No_Element loop
            if Sparse_Exceptions.Key (SE_Cursor) > For_Call then
               Sparse_Exceptions.Previous (SE_Cursor);
            else
               declare
                  E : constant Ada.Exceptions.Exception_Id
                    := Sparse_Exceptions.Element (SE_Cursor);
                  use type Ada.Exceptions.Exception_Id;
               begin
                  if E = Ada.Exceptions.Null_Id then
                     exit;
                  else
                     Ada.Exceptions.Raise_Exception (E, "from stub");
                  end if;
               end;
            end if;
         end loop;
         --  If we drop through, either there was no exception
         --  specified or the specified exception was null.
      end;
   end Check_For_Exception;

   function Get_Output_Value_Stream
     (For_Subprogram_Named : String;
      For_Parameter_Named  : String;
      For_Call             : Positive)
     return Stream_Access is
      SP : constant String := For_Subprogram_Named & "." & For_Parameter_Named;
   begin
      if not Outputs.Contains (SP) then
         raise No_Value with "for output " & SP;
      end if;
      declare
         SO : constant Sparse_Outputs.Map := Outputs.Element (SP);
         SO_Cursor : Sparse_Outputs.Cursor := SO.Last;
         use type Sparse_Outputs.Cursor;
      begin
         while SO_Cursor /= Sparse_Outputs.No_Element loop
            if Sparse_Outputs.Key (SO_Cursor) > For_Call then
               Sparse_Outputs.Previous (SO_Cursor);
            else
               declare
                  package CMS renames Utilities.Memory_Streams;
                  C : constant Output_Cell
                    := Sparse_Outputs.Element (SO_Cursor);
                  Stream : CMS.Stream_Type
                    renames CMS.Stream_Type
                      (Stream_Pointers.Value (C.Stream).all);
                  Copy : CMS.Stream_Type
                    renames CMS.Stream_Type
                      (Stream_Pointers.Value (C.Copy).all);
               begin
                  CMS.Set_Contents (CMS.Contents (Stream), Copy);
                  return Stream_Pointers.Value (C.Copy);
               end;
            end if;
         end loop;
         --  If we drop through, no output was specified.
         raise No_Value with "for output " & SP & " for call" & For_Call'Img;
      end;
   end Get_Output_Value_Stream;

   -------------------------------------------------
   --  L o c a l   i m p l e m e n t a t i o n s  --
   -------------------------------------------------

   function Streamed_Value (From     : Stream_Pointer_Vectors.Vector;
                            At_Index : Positive) return T is
      package AS renames Ada.Streams;
      package CMS renames Scripted_Testing.Utilities.Memory_Streams;
      Orig : CMS.Stream_Type renames
        CMS.Stream_Type (Stream_Pointers.Value
          (From.Element (At_Index)).all);
      Copy :  aliased CMS.Stream_Type
        (Capacity => AS.Stream_Element_Offset (CMS.Length (Orig)));
   begin
      CMS.Set_Contents (CMS.Contents (Orig), Copy);
      return T'Input (Copy'Access);
   end Streamed_Value;

   package body String_Bags is

      not overriding procedure Add
        (B : in out Bag;
         S :        String)
      is
         C : constant String_To_Natural_Maps.Cursor := B.Find (S);
         use type String_To_Natural_Maps.Cursor;
      begin
         if C = String_To_Natural_Maps.No_Element then
            B.Insert (S, (Count => 1));
         else
            B.Replace_Element
              (Position => C,
               New_Item =>
                 (Count =>
                    String_To_Natural_Maps.Element (C).Count + 1));
         end if;
      end Add;

      not overriding function Count
        (B : Bag;
         S : String) return Natural
      is
         C : constant String_To_Natural_Maps.Cursor := B.Find (S);
         use type String_To_Natural_Maps.Cursor;
      begin
         if C = String_To_Natural_Maps.No_Element then
            return 0;
         else
            return String_To_Natural_Maps.Element (C).Count;
         end if;
      end Count;

      not overriding procedure Reset
        (B : in out Bag;
         S :        String)
      is
         C : constant String_To_Natural_Maps.Cursor := B.Find (S);
         use type String_To_Natural_Maps.Cursor;
      begin
         if C /= String_To_Natural_Maps.No_Element then
            B.Replace_Element
              (Position => C,
               New_Item => (Count => 0));
         end if;
      end Reset;

   end String_Bags;

end Scripted_Testing.Stubs;
