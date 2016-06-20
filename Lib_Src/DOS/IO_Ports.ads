-----------------------------------------------------------------------
--
--  File:        io_ports.ads
--  Description: package for reading x86 I/O ports
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

with Interfaces;

package IO_Ports is

   -- Interrupt control
   procedure Enable_Interrupts;
   procedure Disable_Interrupts;

   -- Writing IO ports
   procedure Write_IO_Port (Address : in Interfaces.Unsigned_16;
                            Value   : in Interfaces.Unsigned_8);
   procedure Write_IO_Port (Address : in Interfaces.Unsigned_16;
                            Value   : in Interfaces.Unsigned_16);
   procedure Write_IO_Port (Address : in Interfaces.Unsigned_16;
                            Value   : in Interfaces.Unsigned_32);

   -- Reading IO ports
   procedure Read_IO_Port (Address : in     Interfaces.Unsigned_16;
                           Value   :    out Interfaces.Unsigned_8);
   procedure Read_IO_Port (Address : in     Interfaces.Unsigned_16;
                           Value   :    out Interfaces.Unsigned_16);
   procedure Read_IO_Port (Address : in     Interfaces.Unsigned_16;
                           Value   :    out Interfaces.Unsigned_32);

 private

   pragma Inline (Read_IO_Port);
   pragma Inline (Write_IO_Port);
   pragma Inline (Enable_Interrupts);
   pragma Inline (Disable_Interrupts);

end IO_Ports;
