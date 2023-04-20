--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: CC0-1.0

with Software_Under_Test;
pragma Unreferenced (Software_Under_Test);
with Digital_IO.Scripting;
pragma Unreferenced (Digital_IO.Scripting);

with Scripted_Testing;
with GNAT.OS_Lib;

procedure Demo is
begin
   Scripted_Testing.Start;
   --  Returns on script completion (success or failure).

   --  The SUT will very likely contain a task or tasks; this will
   --  ensure program exit regardless of their state.
   GNAT.OS_Lib.OS_Exit (0);
end Demo;
