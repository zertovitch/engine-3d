-----------------------------------------------------------------------
--
--  File:        djgplibr.ads
--  Description: Interface to DJGPP library functions
--  Rev:         0.7
--  Date:        01-feb-98
--  Author:      Jerry van Dijk
--  Mail:        jdijk@acm.org
--
--  Copyright (c) Jerry van Dijk, 1996, 1997, 1998
--  Billie Holidaystraat 28
--  2324 LK Leiden
--  THE NETHERLANDS
--  tel int + 31 71 531 4365
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-----------------------------------------------------------------------

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

package DJGPP_Library is

   ---------------------------------------------
   -- NAME:    Dpmi_Regs                      --
   --                                         --
   -- PURPOSE: Simplified processor registers --
   ---------------------------------------------
   type Dpmi_Regs is
      record
         Di     : Unsigned_16;
         Di_Hi  : Unsigned_16;
         Si     : Unsigned_16;
         Si_Hi  : Unsigned_16;
         Bp     : Unsigned_16;
         Bp_Hi  : Unsigned_16;
         Res    : Unsigned_16;
         Res_Hi : Unsigned_16;
         Bx     : Unsigned_16;
         Bx_Hi  : Unsigned_16;
         Dx     : Unsigned_16;
         Dx_Hi  : Unsigned_16;
         Cx     : Unsigned_16;
         Cx_Hi  : Unsigned_16;
         Ax     : Unsigned_16;
         Ax_Hi  : Unsigned_16;
         Flags  : Unsigned_16;
         Es     : Unsigned_16;
         Ds     : Unsigned_16;
         Fs     : Unsigned_16;
         Gs     : Unsigned_16;
         Ip     : Unsigned_16;
         Cs     : Unsigned_16;
         Sp     : Unsigned_16;
         Ss     : Unsigned_16;
      end record;
   pragma Convention(C, Dpmi_Regs);


   ----------------------------------------
   -- NAME:    Go32_Info_Block           --
   --                                    --
   -- PURPOSE: Defines the extender info --
   ----------------------------------------
   type Go32_Info_Block is
      record
         Size_Of_This_Structure_In_Bytes        : Unsigned_32;
         Linear_Address_Of_Primary_Screen       : Unsigned_32;
         Linear_Address_Of_Secondary_Screen     : Unsigned_32;
         Linear_Address_Of_Transfer_Buffer      : Unsigned_32;
         Size_Of_Transfer_Buffer                : Unsigned_32;
         Pid                                    : Unsigned_32;
         Master_Interrupt_Controller_Base       : Unsigned_8;
         Slave_Interrupt_Controller_Base        : Unsigned_8;
         Selector_For_Linear_Memory             : Unsigned_16;
         Linear_Address_Of_Stub_Info_Structure  : Unsigned_32;
         Linear_Address_Of_Original_Psp         : Unsigned_32;
         Run_Mode                               : Unsigned_16;
         Run_Mode_Info                          : Unsigned_16;
      end record;
   pragma Convention(C, Go32_Info_Block);


   -------------------------------------------
   -- NAME:    Dos_DS                       --
   --                                       --
   -- PURPOSE: Conventional memory selector --
   -------------------------------------------
   Current_Info : Go32_Info_Block;
   pragma Import(C, Current_Info, "_go32_info_block");

   Dos_DS : Unsigned_16 renames Current_Info.Selector_For_Linear_Memory;


   --------------------------------------------------
   -- NAME:    Dpmi_Int                            --
   --                                              --
   -- PURPOSE: Call a real-mode interrupt          --
   --                                              --
   -- INPUTS:  Vector - Interrupt number           --
   --          Regs   - Processor registers        --
   --                                              --
   -- OUTPUTS: Regs - Modified processor registers --
   --------------------------------------------------
   procedure Dpmi_Int(Vector : in     Unsigned_16;
                      Regs   : in out Dpmi_Regs);
   pragma Import(C, Dpmi_Int, "__dpmi_int");


   ------------------------------------------------------
   -- NAME:    Farsetsel                               --
   --                                                  --
   -- PURPOSE: Preset the conventional memory selector --
   ------------------------------------------------------
   procedure Farsetsel(Selector : in Unsigned_16);
   pragma Import(C, Farsetsel, "_farsetsel");


   -----------------------------------------------------
   -- NAME:    Farpeekb                               --
   --                                                 --
   -- PURPOSE: Reads a byte from memory               --
   --                                                 --
   -- INPUTS:  Selector - The memory segment selector --
   --          Offset   - The fysical address         --
   --                                                 --
   -- RETURNS: The byte at the address                --
   -----------------------------------------------------
   function Farpeekb(Selector : in Unsigned_16;
                     Offset   : in Unsigned_32) return Unsigned_8;
   pragma Import(C, Farpeekb, "_farpeekb");


   -----------------------------------------------
   -- NAME:    Farnspeekb                       --
   --                                           --
   -- PURPOSE: Reads a byte from memory using a --
   --          preset selector                  --
   --                                           --
   -- INPUTS:  Offset - The fysical address     --
   --                                           --
   -- RETURNS: The byte at the address          --
   -----------------------------------------------
   function Farnspeekb(Offset : in Unsigned_32) return Unsigned_8;
   pragma Import(C, Farnspeekb, "_farnspeekb");


   ---------------------------------------------
   -- NAME:    Farpokeb                       --
   --                                         --
   -- PURPOSE: Writes a byte to memory        --
   --                                         --
   -- INPUTS:  Selector - The memory selector --
   --          Offset   - The fysical address --
   --          Value    - The byte to write   --
   ---------------------------------------------
   procedure Farpokeb(Selector : in Unsigned_16;
                      Offset   : in Unsigned_32;
                      Value    : in Unsigned_8);
   pragma Import(C, Farpokeb, "_farpokeb");


   ----------------------------------------------
   -- NAME:    Farnspokeb                      --
   --                                          --
   -- PURPOSE: Writes a byte to memory using a --
   --          preset selector                 --
   --                                          --
   -- INPUTS:  Offset - The fysical address    --
   --          Value  - The byte to write      --
   ----------------------------------------------
   procedure Farnspokeb(Offset : in Unsigned_32;
                        Value  : in Unsigned_8);
   pragma Import(C, Farnspokeb, "_farnspokeb");


   ---------------------------------------------
   -- NAME:    Outportb                       --
   --                                         --
   -- PURPOSE: Outputs a byte to a I/O port   --
   --                                         --
   -- INPUT:   Port - The I/O port            --
   --          Data - The byte to write to it --
   ---------------------------------------------
   procedure Outportb(Port : in Unsigned_16;
                      Data : in Unsigned_8);
   pragma Import(C, Outportb, "outportb");


   ---------------------------------------------------
   -- NAME:    Dosmemget                            --
   --                                               --
   -- PURPOSE: Transfers data from conventional     --
   --          memory to the application            --
   --                                               --
   -- INPUTS:  Offset - Fysical memory address      --
   --          Length - Number of bytes to transfer --
   --          Buffer - The destination             --
   ---------------------------------------------------
   procedure Dosmemget(Offset : in Unsigned_32;
                       Length : in Unsigned_32;
                       Buffer : char_array);
   pragma Import(C, Dosmemget, "dosmemget");


   ---------------------------------------------------
   -- NAME:    Dosmemput                            --
   --                                               --
   -- PURPOSE: Transfers data from the application  --
   --          to conventional memory               --
   --                                               --
   -- INPUTS:  Offset - Fysical memory address      --
   --          Length - Number of bytes to transfer --
   --          Buffer - The destination             --
   ---------------------------------------------------
   procedure Dosmemput(Buffer : char_array;
                       Length : Unsigned_32;
                       Offset : Unsigned_32);
   pragma Import(C, Dosmemput, "dosmemput");


end DJGPP_Library;
