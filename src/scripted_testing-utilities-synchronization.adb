--  Copyright (C) 2000-23 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.0

package body Scripted_Testing.Utilities.Synchronization is

   --  Semaphore  --

   protected body Semaphore_Type is

      entry Seize when not Seized is
      begin
         Seized := True;
      end Seize;

      procedure Release is
      begin
         Seized := False;
      end Release;

      function None_Pending return Boolean is
      begin
         --  See LRM 4.1.4(14).
         return Semaphore_Type.Seize'Count = 0;
      end None_Pending;

   end Semaphore_Type;

   overriding
   procedure Seize (The_Semaphore : in out Semaphore) is
   begin
      The_Semaphore.S.Seize;
   end Seize;

   overriding
   procedure Release (The_Semaphore : in out Semaphore) is
   begin
      The_Semaphore.S.Release;
   end Release;

   overriding
   function None_Pending (On_The_Semaphore : Semaphore) return Boolean is
   begin
      return On_The_Semaphore.S.None_Pending;
   end None_Pending;

   --  Recursive_Semaphore  --

   protected body Recursive_Semaphore_Type is

      entry Seize when True is
         use type Ada.Task_Identification.Task_Id;
      begin
         --  Why do I say Recursive_Semaphore_Type.Seize rather than
         --  just Seize?
         --
         --  Jean-Pascal Cozic <Jean-Pascal.Cozic@ingsud.dga.fr> says:
         --  You have to write Recursive_Semaphore_Type.Seize'Caller
         --  because there are two homographs Seize in the scope (the
         --  entry and the procedure). Using the context, there is no
         --  ambiguity because Caller deals with a prefix that denotes
         --  an entry_declaration, but the compiler cannot use context
         --  in this case.
         --
         --  See LRM 4.1.4(14).
         if Owner = Recursive_Semaphore_Type.Seize'Caller then
            Count := Count + 1;
         else
            requeue Waiting with abort;
         end if;
      end Seize;

      procedure Release is
      begin
         Count := Count - 1;
      end Release;

      function None_Pending return Boolean is
      begin
         return Waiting'Count = 0;
      end None_Pending;

      entry Waiting when Count = 0 is
      begin
         Owner := Waiting'Caller;
         Count := 1;
      end Waiting;

   end Recursive_Semaphore_Type;

   overriding
   procedure Seize (The_Semaphore : in out Recursive_Semaphore) is
   begin
      The_Semaphore.S.Seize;
   end Seize;

   overriding
   procedure Release (The_Semaphore : in out Recursive_Semaphore) is
   begin
      The_Semaphore.S.Release;
   end Release;

   overriding
   function None_Pending
     (On_The_Semaphore : Recursive_Semaphore) return Boolean is
   begin
      return On_The_Semaphore.S.None_Pending;
   end None_Pending;

   --  Single_Monitor  --

   overriding
   procedure Seize_For_Reading (The_Monitor : in out Single_Monitor) is
   begin
      Seize (The_Monitor.The_Semaphore);
   end Seize_For_Reading;

   overriding
   procedure Seize_For_Writing (The_Monitor : in out Single_Monitor) is
   begin
      Seize (The_Monitor.The_Semaphore);
   end Seize_For_Writing;

   overriding
   procedure Release_From_Reading (The_Monitor : in out Single_Monitor) is
   begin
      Release (The_Monitor.The_Semaphore);
   end Release_From_Reading;

   overriding
   procedure Release_From_Writing (The_Monitor : in out Single_Monitor) is
   begin
      Release (The_Monitor.The_Semaphore);
   end Release_From_Writing;

   --  Multiple_Monitor  --

   protected body Monitor_Type is

      entry Seize (Kind : Seize_Kind)
      when Waiting_To_Write'Count = 0 and then not Writing is
      begin
         case Kind is
            when For_Reading =>
               Reader_Count := Reader_Count + 1;
            when For_Writing =>
               requeue Waiting_To_Write with abort;
         end case;
      end Seize;

      procedure Release_From_Reading is
      begin
         Reader_Count := Reader_Count - 1;
      end Release_From_Reading;

      procedure Release_From_Writing is
      begin
         Writing := False;
      end Release_From_Writing;

      entry Waiting_To_Write when Reader_Count = 0 is
      begin
         Writing := True;
      end Waiting_To_Write;

   end Monitor_Type;

   overriding
   procedure Seize_For_Reading (The_Monitor : in out Multiple_Monitor) is
   begin
      The_Monitor.M.Seize (Kind => For_Reading);
   end Seize_For_Reading;

   overriding
   procedure Seize_For_Writing (The_Monitor : in out Multiple_Monitor) is
   begin
      The_Monitor.M.Seize (Kind => For_Writing);
   end Seize_For_Writing;

   overriding
   procedure Release_From_Reading (The_Monitor : in out Multiple_Monitor) is
   begin
      The_Monitor.M.Release_From_Reading;
   end Release_From_Reading;

   overriding
   procedure Release_From_Writing (The_Monitor : in out Multiple_Monitor) is
   begin
      The_Monitor.M.Release_From_Writing;
   end Release_From_Writing;

   --  Lock  --

   overriding
   procedure Initialize (The_Lock : in out Lock) is
   begin
      Seize (The_Lock.Using.all);
   end Initialize;

   overriding
   procedure Finalize (The_Lock : in out Lock) is
   begin
      if not The_Lock.Finalized then
         The_Lock.Finalized := True;
         Release (The_Lock.Using.all);
      end if;
   end Finalize;

   --  Read_Lock  --

   overriding
   procedure Initialize (The_Lock : in out Read_Lock) is
   begin
      Seize_For_Reading (The_Lock.Using.all);
   end Initialize;

   overriding
   procedure Finalize (The_Lock : in out Read_Lock) is
   begin
      if not The_Lock.Finalized then
         The_Lock.Finalized := True;
         Release_From_Reading (The_Lock.Using.all);
      end if;
   end Finalize;

   --  Write_Lock  --

   overriding
   procedure Initialize (The_Lock : in out Write_Lock) is
   begin
      Seize_For_Writing (The_Lock.Using.all);
   end Initialize;

   overriding
   procedure Finalize (The_Lock : in out Write_Lock) is
   begin
      if not The_Lock.Finalized then
         The_Lock.Finalized := True;
         Release_From_Writing (The_Lock.Using.all);
      end if;
   end Finalize;

end Scripted_Testing.Utilities.Synchronization;
