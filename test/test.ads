with Interfaces.C;

with CArgv;
with Tcl;

with Scripted_Testing;

package Test is

   type First is new Scripted_Testing.Command with null record;
   function Tcl_Command
     (C      : access First;
      Interp :        Tcl.Tcl_Interp;
      Argc   :        Interfaces.C.int;
      Argv   :        CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;

end Test;
