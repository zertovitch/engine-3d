-----------------------------------------------------------------------------
--  File: finetime.adb; see specification (finetime.ads)
-----------------------------------------------------------------------------
with DOS_Interrupts;
with Interfaces;                        use Interfaces;
with System.Machine_Code;               use System.Machine_Code;

package body Fine_Timer is

   -- Control latch on the 8454 Programmable Interrupt Timer

   Default_latch: constant:= 16#FFFF#; -- default DOS latch value (18.2 Hz)
   Latch: unsigned_16 := Default_latch;
   PIT_frequency: constant:= 1193180.0;

   procedure Outport (Port: Unsigned_16; Value: Unsigned_8) is
   begin
      ASM ("outb %%dx",
        No_Output_Operands,
        (Unsigned_8'Asm_Input  ("a", Value),
         Unsigned_16'Asm_Input ("d", Port)),
        Volatile => True);
   end;
   pragma Inline (Outport);

   procedure InPort (Address : in     Unsigned_16;
                     Value   :    out Unsigned_8) is
   begin
      ASM ("inb %%dx",
        Unsigned_8'Asm_Output ("=a", Value),
        Unsigned_16'Asm_Input ("d", Address),
        Volatile => True);
   end InPort;
   pragma Inline (InPort);

   procedure Set_counter_latch(value: unsigned_16) is
     begin
       Latch:= value; -- Memorize
       Outport( 16#43#, 16#3C# );
       Outport( 16#40#, unsigned_8( 16#FF# and value ) );
       Outport( 16#40#, unsigned_8( 16#FF# and value/16#100# ) );
     end;

   procedure Set_timer_frequency( Hz: Float ) is
     begin
       Set_counter_latch( unsigned_16( PIT_frequency / Hz ) );
     end;

   procedure Restore_timer_frequency is
     begin
       Set_counter_latch( Default_latch );
     end;

   function BIOS_Time(mode: unsigned_16; new_time: unsigned_32)
             return unsigned_32;
   pragma Import (C, BIOS_Time, "biostime");

  Finest_count: unsigned_32;

  procedure Repair_BIOS_time_with_PTI_ticks is
    -- doesn't work under Win9x DOS box. (seems 2x too fast)
    bt, elapsed_ticks: unsigned_32;
    begin
      elapsed_ticks:= unsigned_32( Finest_count / Default_latch );
      bt:= BIOS_Time(0, 0);
      bt:= BIOS_Time(1, bt + elapsed_ticks); -- !! Midnight...
    end;

  pragma Inline(Repair_BIOS_time_with_PTI_ticks);

  -- Real time clock (RTC)

  Hour, Minute, Second : Unsigned_8:= 0;

  Read_Hours  : constant:= 16#04#;
  Read_Minutes: constant:= 16#02#;
  Read_Seconds: constant:= 16#00#;

  Clock_Register    : constant:= 16#70#;
  Clock_Data        : constant:= 16#71#;

  procedure Repair_BIOS_time_with_RTC is
    bt, elapsed_ticks: unsigned_32;
    begin
      ASM( "cli", Volatile => True );
      OutPort(Clock_Register, Read_Hours);   InPort(Clock_Data, Hour);
      OutPort(Clock_Register, Read_Minutes); InPort(Clock_Data, Minute);
      OutPort(Clock_Register, Read_Seconds); InPort(Clock_Data, Second);
      ASM( "sti", Volatile => True );
      elapsed_ticks:= unsigned_32 (18.206759747 *
        Float(unsigned_32( (Hour and 15)   + 10*(Hour/16)   ) * 3600 +
              unsigned_32( (Minute and 15) + 10*(Minute/16) ) * 60   +
              unsigned_32( (Second and 15) + 10*(Second/16) ) )
        );
      bt:= BIOS_Time(1, elapsed_ticks);
    end;

  pragma Inline(Repair_BIOS_time_with_RTC);

  Seuil: constant unsigned_32:= unsigned_32(PIT_frequency);

  -- Interrupt handler

  -- There are nice partial chaining methods to call the
  -- old interrupt at the old rate, with real & protected mode
  -- interrupts. If you know a *working* one, tell me...
  -- Thus, for now, we don't chain at all: DOS time is updated
  -- "manually".

  -- Repair_BIOS_time_with_RTC method works on
    --  MS-DOS with EMM386
    --  DR-DOS with EMM386 and multitasking
    --  MS-DOS without EMM386
    --  Win95 DOS box
    --  WinNT DOS box

  procedure Handler is
    begin
      Counter:= Counter + 1;
      Finest_count:= Finest_count + unsigned_32( Latch );

      if Finest_count > Seuil then
        Repair_BIOS_time_with_RTC;

        Finest_count:= Finest_count - Seuil;
      end if;

      Outport (16#20#, 16#20#); -- Send End-of-Interrupt to PIC
    end Handler;

  Timer_int: constant := 16#08#;   -- timer interrupt (alt. 16#1C#)

  -- Installation

  installed: Boolean:= False;

  procedure Install is
    begin
      Counter:= 0;
      Finest_count:= 0;
      Restore_timer_frequency;
      if not installed then
        Dos_Interrupts.Set_Interrupt (Timer_int, Handler'Address);
        installed:= True;
      end if;
    end;

  procedure Uninstall is
    begin
      if installed then
        Dos_Interrupts.Remove_Interrupt (Timer_int);
        installed:= False;
      end if;
      Restore_timer_frequency;
      Repair_BIOS_time_with_RTC;
    end;

 end Fine_Timer;
