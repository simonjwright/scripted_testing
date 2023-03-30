--  Copyright (C) 2000-23 Simon Wright <simon@pushface.org>
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.0

--  This package was originally developed as part of the Ada 95 Booch
--  Components.

with Ada.Finalization;
with Ada.Task_Identification;

package Scripted_Testing.Utilities.Synchronization is

   pragma Elaborate_Body;

   --  Semaphores provide for mutual exclusion.
   type Semaphore_Base is abstract tagged limited private;
   procedure Seize (The_Semaphore : in out Semaphore_Base) is abstract;
   procedure Release (The_Semaphore : in out Semaphore_Base) is abstract;
   function None_Pending (On_The_Semaphore : Semaphore_Base) return Boolean
      is abstract;

   --  A Semaphore is like a standard POSIX mutex.
   type Semaphore is new Semaphore_Base with private;
   overriding
   procedure Seize (The_Semaphore : in out Semaphore);
   overriding
   procedure Release (The_Semaphore : in out Semaphore);
   overriding
   function None_Pending (On_The_Semaphore : Semaphore) return Boolean;

   --  A Recursive_Semaphore is like a POSIX recursive mutex; once
   --  Seized by a task, that task can Seize again; other tasks are
   --  blocked until the owning task has Released the semaphore as
   --  many times as it Seized it.
   type Recursive_Semaphore is new Semaphore_Base with private;
   overriding
   procedure Seize (The_Semaphore : in out Recursive_Semaphore);
   overriding
   procedure Release (The_Semaphore : in out Recursive_Semaphore);
   overriding
   function None_Pending
     (On_The_Semaphore : Recursive_Semaphore) return Boolean;

   --  Monitors support Locks.
   type Monitor_Base is abstract tagged limited private;
   procedure Seize_For_Reading (The_Monitor : in out Monitor_Base)
      is abstract;
   procedure Seize_For_Writing (The_Monitor : in out Monitor_Base)
      is abstract;
   procedure Release_From_Reading (The_Monitor : in out Monitor_Base)
      is abstract;
   procedure Release_From_Writing (The_Monitor : in out Monitor_Base)
      is abstract;

   --  Single_Monitors allow one task at a time to have access, be it
   --  for reading or writing.
   type Single_Monitor is new Monitor_Base with private;
   overriding
   procedure Seize_For_Reading (The_Monitor : in out Single_Monitor);
   overriding
   procedure Seize_For_Writing (The_Monitor : in out Single_Monitor);
   overriding
   procedure Release_From_Reading (The_Monitor : in out Single_Monitor);
   overriding
   procedure Release_From_Writing (The_Monitor : in out Single_Monitor);

   --  Multiple_Monitors allow multiple readers; however, when a
   --  writer owns the monitor it has exclusive access.
   type Multiple_Monitor is new Monitor_Base with private;
   overriding
   procedure Seize_For_Reading (The_Monitor : in out Multiple_Monitor);
   overriding
   procedure Seize_For_Writing (The_Monitor : in out Multiple_Monitor);
   overriding
   procedure Release_From_Reading (The_Monitor : in out Multiple_Monitor);
   overriding
   procedure Release_From_Writing (The_Monitor : in out Multiple_Monitor);

   --  A Lock is designed to provide "locking by declaration".
   --    declare
   --      L : Lock (Some_Monitor'Access);
   --    begin
   --      -- the monitor is locked
   --    end;
   --    -- the monitor is unlocked as L is finalized, even if an exception
   --    -- occurs

   type Lock_Base
      is abstract new Ada.Finalization.Limited_Controlled with private;

   --  A simple Lock provides mutual exclusion
   type Lock (Using : access Semaphore_Base'Class)
   is new Lock_Base with private;

   --  Read_ and Write_ Locks support multiple reader/single writer
   --  access provided the given Monitor supports it; otherwise they
   --  merely provide mutual exclusion.
   type Read_Lock (Using : access Monitor_Base'Class)
   is new Lock_Base with private;

   type Write_Lock (Using : access Monitor_Base'Class)
   is new Lock_Base with private;

private

   type Semaphore_Base is abstract tagged limited null record;

   protected type Semaphore_Type is
      entry Seize;
      procedure Release;
      function None_Pending return Boolean;
   private
      Seized : Boolean := False;
   end Semaphore_Type;

   type Semaphore is new Semaphore_Base with record
      S : Semaphore_Type;
   end record;

   protected type Recursive_Semaphore_Type is
      entry Seize;
      procedure Release;
      function None_Pending return Boolean;
   private
      entry Waiting;
      Owner : Ada.Task_Identification.Task_Id;
      Count : Natural := 0;
   end Recursive_Semaphore_Type;

   type Recursive_Semaphore is new Semaphore_Base with record
      S : Recursive_Semaphore_Type;
   end record;

   type Monitor_Base is abstract tagged limited null record;

   type Single_Monitor is new Monitor_Base with record
      The_Semaphore : Recursive_Semaphore;
   end record;

   --  Monitor_Type is due to Matthew Heaney <matthew_heaney@acm.org>.
   --  The Booch C++ version was inoperative, at least in sjw's translation.

   type Seize_Kind is (For_Reading, For_Writing);

   protected type Monitor_Type is
      entry Seize (Kind : Seize_Kind);
      procedure Release_From_Reading;
      procedure Release_From_Writing;
   private
      entry Waiting_To_Write;
      Reader_Count : Natural := 0;
      Writing : Boolean := False;
   end Monitor_Type;

   type Multiple_Monitor is new Monitor_Base with record
      M : Monitor_Type;
   end record;

   type Lock_Base
   is abstract new Ada.Finalization.Limited_Controlled with record
      Finalized : Boolean := False;
   end record;

   type Lock (Using : access Semaphore_Base'Class)
   is new Lock_Base with null record;
   overriding
   procedure Initialize (The_Lock : in out Lock);
   overriding
   procedure Finalize (The_Lock : in out Lock);

   type Read_Lock (Using : access Monitor_Base'Class)
   is new Lock_Base with null record;
   overriding
   procedure Initialize (The_Lock : in out Read_Lock);
   overriding
   procedure Finalize (The_Lock : in out Read_Lock);

   type Write_Lock (Using : access Monitor_Base'Class)
   is new Lock_Base with null record;
   overriding
   procedure Initialize (The_Lock : in out Write_Lock);
   overriding
   procedure Finalize (The_Lock : in out Write_Lock);

end Scripted_Testing.Utilities.Synchronization;
