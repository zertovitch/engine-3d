-----------------------------------------------------------------------
--
--  File:        io_ports.adb
--  Description: package for reading x86 I/O ports - GNAT 3.07
--  Rev:         0.1
--  Date:        19-nov-1997
--  Author:      Jerry van Dijk
--  Mail:        jdijk@acm.org
--
--  Copyright (c) Jerry van Dijk, 1997
--  Billie Holidaystraat 28
--  2324 LK  LEIDEN
--  THE NETHERLANDS
--  tel int + 31 71 531 43 65
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-----------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;

package body IO_Ports is

   use Interfaces;

   -----------------------
   -- INTERRUPT CONTROL --
   -----------------------

   procedure Enable_Interrupts is
   begin
      Asm ("STI", Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      Asm ("CLI", Volatile => True);
   end Disable_Interrupts;


   ----------------------
   -- READING IO PORTS --
   ----------------------

   procedure Read_IO_Port (Address : in     Unsigned_16;
                           Value   :    out Unsigned_8) is
   begin
      ASM ("INB %%DX",
        Unsigned_8'Asm_Output ("=a", Value),
        Unsigned_16'Asm_Input ("d", Address),
        Volatile => True);
   end Read_IO_Port;

   procedure Read_IO_Port (Address : in     Unsigned_16;
                           Value   :    out Unsigned_16) is
   begin
      ASM ("INW %%DX",
        Unsigned_16'Asm_Output ("=a", Value),
        Unsigned_16'Asm_Input  ("d", Address),
        Volatile => True);
   end Read_IO_Port;

   procedure Read_IO_Port (Address : in     Unsigned_16;
                           Value   :    out Unsigned_32) is
   begin
      ASM ("INL %%DX",
        Unsigned_32'Asm_Output ("=a", Value),
        Unsigned_16'Asm_Input  ("d", Address),
        Volatile => True);
   end Read_IO_Port;


   ----------------------
   -- WRITING IO PORTS --
   ----------------------

   procedure Write_IO_Port (Address : in Unsigned_16;
                            Value   : in Unsigned_8) is
   begin
      ASM ("OUTB %%DX",
        No_Output_Operands,
        (Unsigned_8'Asm_Input  ("a", Value),
         Unsigned_16'Asm_Input ("d", Address)),
       Volatile => True);
   end Write_IO_Port;

   procedure Write_IO_Port (Address : in Unsigned_16;
                            Value   : in Unsigned_16) is
   begin
      ASM ("OUTW %%DX",
        No_Output_Operands,
        (Unsigned_16'Asm_Input ("a", Value),
         Unsigned_16'Asm_Input ("d", Address)),
       Volatile => True);
   end Write_IO_Port;

   procedure Write_IO_Port (Address : in Unsigned_16;
                            Value   : in Unsigned_32) is
   begin
      ASM ("OUTL %%DX",
        No_Output_Operands,
        (Unsigned_32'Asm_Input ("a", Value),
         Unsigned_16'Asm_Input ("d", Address)),
       Volatile => True);
   end Write_IO_Port;

end IO_Ports;
