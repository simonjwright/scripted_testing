--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: CC0-1.0

generic
   type T is private;
package Sample_Callbacks is

   --  The Callback Procedure type
   type Callback is not null access procedure (The_T : T);

   function Is_Registered (Proc : Callback) return Boolean;

   --  Called to register Proc to receive callbacks
   procedure Register (Proc : Callback)
   with
     Pre  => not Is_Registered (Proc),
     Post => Is_Registered (Proc);

   --  Called to stop Proc receiving callbacks
   procedure Deregister (Proc : Callback)
   with
     Pre  => Is_Registered (Proc),
     Post => not Is_Registered (Proc);

   --  Call all the registered callback procedures with With_Param.
   procedure Call_Callbacks (With_Param : T);

   --  Clear all registered callbacks
   procedure Clear;

end Sample_Callbacks;
