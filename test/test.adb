--  Copyright Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  This package is distributed
--  in the hope that it will be useful, but WITHOUT ANY WARRANTY;
--  without even the implied warranty of MERCHANTABILITY or FITNESS
--  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
--  more details.  You should have received a copy of the GNU General
--  Public License distributed with this package; see file COPYING3.
--  If not, go to http://www.gnu.org/licenses for a complete copy of
--  the license.

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with CArgv;
with Interfaces.C;
with Scripted_Testing;
with Tcl.Ada;

package body Test is

   function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   package First is
   private
      procedure Initialize;
   end First;
   package body First is separate;

   package Except is
   private
      procedure Initialize;
   end Except;
   package body Except is separate;

   package Lists is
   private
      procedure Initialize;
   end Lists;
   package body Lists is separate;

end Test;
