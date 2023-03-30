--  Copyright (C) 2002-23 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.0

--  This package was originally developed as part ot the Ada 95 Booch
--  Components.

with Ada.Streams;

package Scripted_Testing.Utilities.Memory_Streams is

   --  pragma Preelaborate;

   type Stream_Type
     (Capacity : Ada.Streams.Stream_Element_Count)
      is new Ada.Streams.Root_Stream_Type with private;
   --  Provides an in-memory Stream.

   function Length (Stream : Stream_Type) return Natural;
   --  Returns the number of stream elements in Stream.

   function Contents (Stream : Stream_Type)
                     return Ada.Streams.Stream_Element_Array;
   --  Returns a copy of the contents of Stream.

   procedure Write_Contents (To : access Ada.Streams.Root_Stream_Type'Class;
                             Stream : Stream_Type);
   --  Writes the contents of Stream directly to the stream To.
   --
   --  If To is itself a memory Stream_Type, this will effectively
   --  concatenate Stream to To.

   procedure Read_Contents (From : access Ada.Streams.Root_Stream_Type'Class;
                            Stream : in out Stream_Type);
   --  Fills Stream directly from the stream From.
   --
   --  Reads the lesser of the Capacity of Stream and the "length" of
   --  From.
   --
   --  The previous contents of Stream are lost.

   procedure Set_Contents (From : Ada.Streams.Stream_Element_Array;
                           Stream : in out Stream_Type);
   --  Sets the contents of Stream to be the contents of array From.
   --
   --  Raises Ada.IO_Exceptions.End_Error if Stream is not
   --  large enough to contain From.
   --
   --  The previous contents of Stream are lost.
   --
   --  Aimed at use with datagram sockets, where you have to take the
   --  contents in one bite and can't know in advance how long the
   --  datagram is.

   procedure Reset (Stream : out Stream_Type);
   --  Clears Stream.

private

   type Stream_Type
     (Capacity : Ada.Streams.Stream_Element_Count)
   is new Ada.Streams.Root_Stream_Type with record
      Next_Write : Ada.Streams.Stream_Element_Count := 1;
      Next_Read : Ada.Streams.Stream_Element_Count := 1;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Capacity);
   end record;

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Removes Item'Length storage elements (or, as many as remain)
   --  from Stream. Last is updated to the final index in Item that
   --  was updated (normally, Item'Last). When Stream was already
   --  empty, Item will be unchanged and Last will be set to
   --  Item'First - 1.

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array);
   --  Adds Item to Stream. Raises Ada.IO_Exceptions.End_Error on
   --  overrun.

end Scripted_Testing.Utilities.Memory_Streams;
