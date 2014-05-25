with Ada.Text_IO; use Ada.Text_IO;

package body Test is

   function Tcl_Command
     (C      : access First;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      use type Interfaces.C.int;
   begin
      Put_Line ("'first' called from Ada.");
      for J in 0 .. Argc - 1 loop
         Put_Line ("arg" & J'Img & " " & CArgv.Arg (Argv, J));
      end loop;
      return Tcl.TCL_OK;
   end Tcl_Command;

end Test;
