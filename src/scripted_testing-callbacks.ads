--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later

--  Creates a Tcl command callback-<callback_type_name> (lowercased)
--  which takes one argument: the value to be provided. If the type
--  has more than one component, it is to be passed as a list,
--  {component-1 component-2 ...}.

with Scripted_Testing.Callbacks_Signature;

generic
   type T is private;
   Callback_Type_Name : String;
   with function Value (S : String) return T is <>;
   with package Application_Callbacks
     is new Scripted_Testing.Callbacks_Signature (T => T, Call_Callbacks => <>);
package Scripted_Testing.Callbacks with Elaborate_Body is
end Scripted_Testing.Callbacks;
