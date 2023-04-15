--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later

generic
   type T is private;
   with procedure Call_Callbacks (With_Value : T);
package Scripted_Testing.Callbacks_Signature is
end Scripted_Testing.Callbacks_Signature;
