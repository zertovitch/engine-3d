-----------------------------------------------------------------------
--
--  File:        dosinter.adb
--  Description: package for handling interrupts on DOS - GNAT 3.10
--  Rev:         0.1
--  Date:        13-jul-1998
--  Author:      Jerry van Dijk
--  Mail:        jdijk@acm.org
--
--  Copyright (c) Jerry van Dijk, 1997
--  Billie Holidaystraat 28
--  2324 LK  LEIDEN
--  THE NETHERLANDS
--  tel int +31 (0)71 531 43 65
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body DOS_Interrupts is

   use type Interfaces.unsigned_32;

   -------------------------------
   -- interface to dos extender --
   -------------------------------

   type Dpmi_Seginfo is
      record
         Size        : Interfaces.unsigned_32;
         Pm_Offset   : System.Address;
         Pm_Selector : Interfaces.unsigned_16;
         Rm_Offset   : Interfaces.unsigned_16;
         Rm_Segment  : Interfaces.unsigned_16;
      end record;
   pragma Convention (C, Dpmi_Seginfo);

   type Dpmi_Seginfo_Ptr is access all Dpmi_Seginfo;
   pragma Convention (C, Dpmi_Seginfo_Ptr);

   procedure Free is new
     Ada.Unchecked_Deallocation (Dpmi_Seginfo, Dpmi_Seginfo_Ptr);

   function Get_Interrupt_Vector (Vector   : Integer;
                                  Addr_Ptr : Dpmi_Seginfo_Ptr) return Integer;
   pragma Import (C, Get_Interrupt_Vector,
     "_go32_dpmi_get_protected_mode_interrupt_vector");

   function Set_Interrupt_Vector (Vector   : Integer;
                                  Addr_Ptr : Dpmi_Seginfo_Ptr) return Integer;
   pragma Import (C, Set_Interrupt_Vector,
     "_go32_dpmi_set_protected_mode_interrupt_vector");

   function Chain_Interrupt_Vector (Vector   : Integer;
                                    Addr_Ptr : Dpmi_Seginfo_Ptr) return Integer;
   pragma Import (C, Chain_Interrupt_Vector,
     "_go32_dpmi_chain_protected_mode_interrupt_vector");

   function Allocate_Iret_Wrapper (Info : Dpmi_Seginfo_Ptr) return Integer;
   pragma Import (C, Allocate_Iret_Wrapper,
     "_go32_dpmi_allocate_iret_wrapper");

   function Free_Iret_Wrapper (Info : Dpmi_Seginfo_Ptr) return Integer;
   pragma Import (C, Free_Iret_Wrapper,
     "_go32_dpmi_free_iret_wrapper");
   CRT0_FLAG_LOCK_MEMORY : constant := 16#1000#;

   Crt0_Startup_Flags : Interfaces.unsigned_32;
   pragma Import (C, Crt0_Startup_Flags, "_crt0_startup_flags");

   Cs_Value : Interfaces.unsigned_16;
   pragma Import (C, Cs_Value, "_go32_my_cs");

   ---------------------------
   -- interrupt bookkeeping --
   ---------------------------
   Old_Ints   : array (Interfaces.unsigned_8) of Dpmi_Seginfo_Ptr;
   New_Ints   : array (Interfaces.unsigned_8) of Dpmi_Seginfo_Ptr;
   Chain_ints : array (Interfaces.unsigned_8) of Boolean := (others => False);

   ---------------------------------------
   -- set interrupt to call the Handler --
   ---------------------------------------
   procedure Set_Interrupt (
     Int     : in Interfaces.unsigned_8;
     Handler : in Interrupt_Handler)
   is
      Result : Integer;
   begin
      if Old_Ints(Int) /= null then
         raise Interrupt_In_Use_Error;
      end if;
      Old_Ints(Int) := new Dpmi_Seginfo;
      Result := Get_Interrupt_Vector (Integer (Int), Old_Ints(Int));
      New_Ints(Int) := new Dpmi_Seginfo;
      New_Ints(Int).all := Old_Ints(Int).all;
      New_Ints(Int).Pm_Selector := Cs_Value;
      New_Ints(Int).Pm_Offset   := Handler;
      if Allocate_Iret_Wrapper (New_Ints(Int)) /= 0 then
         Free (New_Ints(Int));
         Free (Old_Ints(Int));
         raise Set_Interrupt_Error;
      end if;
      if Set_Interrupt_Vector (Integer (Int), New_Ints(Int)) /= 0 then
         Free (New_Ints(Int));
         Free (Old_Ints(Int));
         raise Set_Interrupt_Error;
      end if;
      Chain_Ints(Int) := False;
   end Set_Interrupt;

   ----------------------------------------
   -- set interrupt to chain the Handler --
   ----------------------------------------
   procedure Chain_Interrupt (
     Int     : in Interfaces.unsigned_8;
     Handler : in Interrupt_Handler)
   is
      Result : Integer;
   begin
      if Old_Ints(Int) /= null then
         raise Interrupt_In_Use_Error;
      end if;
      Old_Ints(Int) := new Dpmi_Seginfo;
      Result := Get_Interrupt_Vector (Integer (Int), Old_Ints(Int));
      New_Ints(Int) := new Dpmi_Seginfo;
      New_Ints(Int).all := Old_Ints(Int).all;
      New_Ints(Int).Pm_Selector := Cs_Value;
      New_Ints(Int).Pm_Offset   := Handler;
      if Chain_Interrupt_Vector (Integer (Int), New_Ints(Int)) /= 0 then
         Free (New_Ints(Int));
         Free (Old_Ints(Int));
         raise Set_Interrupt_Error;
      end if;
      Chain_Ints(Int) := True;
   end Chain_Interrupt;

   -------------------------------------------
   -- remove the handler from the interrupt --
   -------------------------------------------
   procedure Remove_Interrupt (Int : Interfaces.unsigned_8)
   is
      Result : Integer;
   begin
      if Old_Ints(Int) = null then
         raise Interrupt_Not_Used_Error;
      end if;
      if Set_Interrupt_Vector (Integer (Int), Old_Ints(Int)) /= 0 then
         raise Remove_Interrupt_Error;
      end if;
      if Chain_Ints(Int) = False then
         if Free_Iret_Wrapper (New_Ints(Int)) /= 0 then
            Result := Set_Interrupt_Vector (Integer (Int), New_Ints(Int));
            raise Remove_Interrupt_Error;
         end if;
      end if;
      Chain_Ints(Int) := False;
      Free (New_Ints(Int));
      Free (Old_Ints(Int));
   end Remove_Interrupt;

begin

   ---------------------------
   -- disable page swapping --
   ---------------------------
   Crt0_Startup_Flags := Crt0_Startup_Flags or CRT0_FLAG_LOCK_MEMORY;

end DOS_Interrupts;
