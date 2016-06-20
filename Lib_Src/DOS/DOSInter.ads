-----------------------------------------------------------------------
--
--  File:        dosinter.ads
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

  --------------------------------------------------------------------
  -- This package disables page swapping, so should only be used    --
  -- if enought physical memory is available for the program.       --
  -- It installs only protected mode interrupts, so the interrupt   --
  -- frequency is limited to about 10KHz.                           --
  -- Note that an interrupt can be redirected or chained only once. --
  -- Also note that the interrupt handler does not have parameters. --
  -- Finally, to handle hardware interrupts, you have to know where --
  -- the hardware IRQ is mapped into the interrupt table!           --
  --------------------------------------------------------------------

with System;
with Interfaces;

package DOS_Interrupts is

   subtype Interrupt_Handler is System.Address;
   -- prototype for an interrupt handling procedure

   procedure Set_Interrupt (Int     : in Interfaces.unsigned_8;
                            Handler : in Interrupt_Handler);
   -- set interrupt to call the Handler

   procedure Chain_Interrupt (Int     : in Interfaces.unsigned_8;
                              Handler : in Interrupt_Handler);
   -- set interrupt to chain the Handler

   procedure Remove_Interrupt (Int : in Interfaces.unsigned_8);
   -- remove the handler from the interrupt

   Set_Interrupt_Error      : exception;
   Remove_Interrupt_Error   : exception;
   Interrupt_In_Use_Error   : exception;
   Interrupt_Not_Used_Error : exception;

end DOS_Interrupts;
