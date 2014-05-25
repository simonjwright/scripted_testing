with Ada.Text_IO; use Ada.Text_IO;

package body Test is

   function Tcl_Command
     (C      : access First;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
      is
   begin
      Put_Line ("'first' called from Ada.");
      return Tcl.TCL_OK;
   end Tcl_Command;

end Test;
