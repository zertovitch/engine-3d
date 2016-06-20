-----------------------------------------------------------------------------
--  File: sbdriver.adb; see specification (sbdriver.ads)
-----------------------------------------------------------------------------
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.IO_Exceptions;                 use Ada.IO_Exceptions;
with System.Machine_Code;               use System.Machine_Code;
with GNAT.OS_lib, DOS_Interrupts;
with DJGPP_Library;                     use DJGPP_Library;
with Interfaces;                        use Interfaces;

with SB_Digi;

package body SB_Driver is
  use ASCII;

  function Inport (Port : Unsigned_16) return Unsigned_8 is
     Value : Unsigned_8;
  begin
     ASM ("inb %%dx",
       Unsigned_8'Asm_Output ("=a", Value),
       Unsigned_16'Asm_Input ("d", Port),
       Volatile => True);
     return Value;
  end Inport;
  pragma Inline (Inport);

  procedure Outport (Port : in Unsigned_16; Value : in Unsigned_8) is
  begin
     ASM ("outb %%dx",
       No_Output_Operands,
       (Unsigned_8'Asm_Input  ("a", Value),
        Unsigned_16'Asm_Input ("d", Port)),
       Volatile => True);
  end Outport;
  pragma Inline (Outport);

  function DOS_Alloc(paragraphs  : in  Unsigned_16;
                     selector_add: in  System.Address ) return Unsigned_16;
  pragma Import(C, DOS_Alloc, "__dpmi_allocate_dos_memory");

  procedure DOS_Free(selector: Unsigned_16);
  pragma Import(C, DOS_Free, "__dpmi_free_dos_memory");

  ----------------------------------------------
  --------| DMA package for sound card |--------
  ----------------------------------------------

  package DMA is
    -- Pass: Linear address of transfer buffer (DWORD);
    --       size of transfer buffer, minus 1 (in BYTES)
    procedure Read_8bit_Single_Cycle( linearAddressOfBuffer: unsigned_32;
                                      length: Integer );
    procedure Read_8bit_AutoInit( linearAddressOfBuffer: unsigned_32;
                                  length: Integer );

    -- int sb_dma_module_lock_your_memory(void);
  end DMA;

  package body DMA is

    generic
      dmaCommand: Unsigned_8;
    procedure Program_DMA( linearAddressOfBuffer: unsigned_32;
                           length: Integer );

    procedure Program_DMA( linearAddressOfBuffer: unsigned_32;
                           length: Integer ) is

      pagePort: constant array( unsigned_8'(0)..7 ) of
                unsigned_16:= ( 16#87#, 16#83#, 16#81#, 16#82#,
                                    -1, 16#8B#, 16#89#, 16#8A# );
      -- ^ "DMAPage" in CT demos

      offset, page, port : Unsigned_16;

      DMA8_FF_REG  : constant:= 16#000C#;
      DMA8_MASK_REG: constant:= 16#000A#;
      DMA8_MODE_REG: constant:= 16#000B#;

      begin
        page:=   Unsigned_16( linearAddressOfBuffer  /  65536 );
        offset:= Unsigned_16( linearAddressOfBuffer mod 65536 );

        Outport( DMA8_MASK_REG, info.DMA or 16#04#); -- Mask channel
        Outport( DMA8_FF_REG,   0);             -- Clear byte flip-flop
        Outport( DMA8_MODE_REG, info.DMA or dmaCommand); -- Set mode

        port:= unsigned_16(info.DMA) * 2;
        Outport(port,  Unsigned_8(offset mod 256)); -- Program offset
        Outport(port,  Unsigned_8(offset  /  256));

        port:= port + 1;
        Outport(port,  Unsigned_8(length mod 256)); -- Program length
        Outport(port,  Unsigned_8(length  /  256));

        Outport( pagePort(info.DMA),Unsigned_8(page)); -- Program page
        Outport( DMA8_MASK_REG, info.DMA); -- Clear mask -> re-enable DMA

    end Program_DMA;

    procedure R8AI is new Program_DMA( 16#58# );
    procedure R8SC is new Program_DMA( 16#48# );

    procedure Read_8bit_AutoInit( linearAddressOfBuffer: unsigned_32;
                                  length: Integer )
      renames R8AI;

    procedure Read_8bit_Single_Cycle( linearAddressOfBuffer: unsigned_32;
                                      length: Integer )
      renames R8SC;

    -- (for virtual memory: lock procedures, pagePort!)
  end DMA;

  ------------------------------------------------------------------------
  --------| Sound Blaster DSP (Digital Signal Processor) package |--------
  ------------------------------------------------------------------------
  package DSP is

    procedure Hard_Reset;

    procedure Write( v: unsigned_8 ); -- inl.
    function Read return unsigned_8;

    procedure Set_time_constant( freq_for_DMA_transfers: Positive );
    procedure Set_high_speed_time_const( freq: Positive );

    procedure Start_DMA_transfer_Single_Cycle( Size_of_tr_buffer_m_1: unsigned_16 );
    procedure Start_DMA_transfer_Auto_Init( Size_of_tr_buffer_m_1: unsigned_16 );

    procedure Start_high_speed_DMA_transfer_Auto_Init(
                 Size_of_tr_buffer_m_1: unsigned_16 );

    -- int sb_dsp_module_lock_your_memory(void);

    pragma Inline( Write, Read );

  end DSP;

  ---------------------------------------------------
  --------| Sound Blaster detection package |--------
  ---------------------------------------------------
  package SB_Detect is

    procedure Check_SB_card_presence;
    -- NB: raises an exception if anything have gone wrong.
    no_blaster_var_cannot_detect: exception;

    -- Pass: New DMA channel. Default is 1, and the
    --       Check_SB_card_presence won't find it if it's anywhere else.
    -- procedure Change_DMA_channel( new_dma: Natural );

  end SB_Detect;

  package body SB_Detect is

    --  testCount  : integer; -- volatile

    -- Internal

      -- SB Detection via interrupts (later!)

    --    -----------------------------------------------------------
    --
    --   generic IRQ_id: integer; procedure Test_interrupt;
    --   procedure Test_interrupt is
    --     Dummy: unsigned_8;
    --   begin
    --     testCount:= IRQ_id;
    --     Dummy:= Inport( sb_info.dataAvail );
    --     Outport(16#20#, 16#20#);
    --   end Test_interrupt;
    --
    --   procedure Test_Int_2  is new Test_interrupt( IRQ_id => 2  );
    --   procedure Test_Int_3  is new Test_interrupt( IRQ_id => 3  );
    --   procedure Test_Int_5  is new Test_interrupt( IRQ_id => 5  );
    --   procedure Test_Int_7  is new Test_interrupt( IRQ_id => 7  );
    --   procedure Test_Int_10 is new Test_interrupt( IRQ_id => 10 );
    --
    -- -- Must be locked in RAM
    --
    -- static int findInterrupt(void) {
    --   __dpmi_paddr old2, old3, old5, old7, old10;
    --   __dpmi_paddr new2, new3, new5, new7, new10;
    --   _go32_dpmi_seginfo wrap2, wrap3, wrap5, wrap7, wrap10;
    --   BYTE pic1Default, pic2Default;
    --
    --   testCount=0;
    --
    --   if(!_go32_dpmi_lock_code(topOfFunctions,((char *)bottomOfFunctions-(char *)topOfFunctions))) {
    --     if(_go32_dpmi_lock_data(&testCount,sizeof(int)))
    --       return 0;
    --   }
    --   else
    --     return 0;
    --
    --   __dpmi_get_protected_mode_interrupt_vector(0x0A,&old2);
    --   __dpmi_get_protected_mode_interrupt_vector(0x0B,&old3);
    --   __dpmi_get_protected_mode_interrupt_vector(0x0D,&old5);
    --   __dpmi_get_protected_mode_interrupt_vector(0x0F,&old7);
    --   __dpmi_get_protected_mode_interrupt_vector(0x72,&old10);
    --
    --   wrap2.pm_offset=(int)testInt2;
    --   wrap2.pm_selector=_my_cs();
    --   wrap3.pm_offset=(int)testInt3;
    --   wrap3.pm_selector=_my_cs();
    --   wrap5.pm_offset=(int)testInt5;
    --   wrap5.pm_selector=_my_cs();
    --   wrap7.pm_offset=(int)testInt7;
    --   wrap7.pm_selector=_my_cs();
    --   wrap10.pm_offset=(int)testInt10;
    --   wrap10.pm_selector=_my_cs();
    --
    --   _go32_dpmi_allocate_iret_wrapper(&wrap2);
    --   _go32_dpmi_allocate_iret_wrapper(&wrap3);
    --   _go32_dpmi_allocate_iret_wrapper(&wrap5);
    --   _go32_dpmi_allocate_iret_wrapper(&wrap7);
    --   _go32_dpmi_allocate_iret_wrapper(&wrap10);
    --
    --   new2.offset32=wrap2.pm_offset;
    --   new2.selector=wrap2.pm_selector;
    --   new3.offset32=wrap3.pm_offset;
    --   new3.selector=wrap3.pm_selector;
    --   new5.offset32=wrap5.pm_offset;
    --   new5.selector=wrap5.pm_selector;
    --   new7.offset32=wrap7.pm_offset;
    --   new7.selector=wrap7.pm_selector;
    --   new10.offset32=wrap10.pm_offset;
    --   new10.selector=wrap10.pm_selector;
    --
    --   pic1Default=inportb(0x21);
    --   pic2Default=inportb(0xA1);
    --
    --   outportb(0x21,pic1Default&0x53);      /* Clear all relevent masks */
    --   outportb(0xA1,pic2Default&0xFB);
    --   __dpmi_set_protected_mode_interrupt_vector(0x0A,&new2);
    --   __dpmi_set_protected_mode_interrupt_vector(0x0B,&new3);
    --   __dpmi_set_protected_mode_interrupt_vector(0x0D,&new5);
    --   __dpmi_set_protected_mode_interrupt_vector(0x0F,&new7);
    --   __dpmi_set_protected_mode_interrupt_vector(0x72,&new10);
    --
    --   DSP.Write(0xF2);             -- This will force the DSP to signal
    --   while(testCount==0);         -- a hardware interrupt, which one of
    --                                -- the functions I set up will handle.
    --   outportb(0x21,pic1Default);
    --   outportb(0xA1,pic2Default);
    --   __dpmi_set_protected_mode_interrupt_vector(0x0A,&old2);
    --   __dpmi_set_protected_mode_interrupt_vector(0x0B,&old3);
    --   __dpmi_set_protected_mode_interrupt_vector(0x0D,&old5);
    --   __dpmi_set_protected_mode_interrupt_vector(0x0F,&old7);
    --   __dpmi_set_protected_mode_interrupt_vector(0x72,&old10);
    --
    --   _go32_dpmi_free_iret_wrapper(&wrap2);
    --   _go32_dpmi_free_iret_wrapper(&wrap3);
    --   _go32_dpmi_free_iret_wrapper(&wrap5);
    --   _go32_dpmi_free_iret_wrapper(&wrap7);
    --   _go32_dpmi_free_iret_wrapper(&wrap10);
    --
    --   return testCount;
    -- }
    --
    -- static sb_status findSoundBlaster(void) {
    --   static WORD ioaddr[7] = { 0x220, 0x240, 0x210, 0x230, 0x250, 0x260, 0x280 };
    --   int a;
    --   sb_status stat=SB_FAILURE;
    --
    --   for(a=0;a<7;a++) {
    --     sb_info.reset=ioaddr[a]+0x06;
    --     sb_info.readData=ioaddr[a]+0x0A;
    --     sb_info.writeData=ioaddr[a]+0x0C;
    --     sb_info.dataAvail=ioaddr[a]+0x0E;
    --
    --     if(DSP.Hard_Reset()) {                 /* Found the right IO address! */
    --       a=7;
    --       if(sb_info.IRQ=findInterrupt()) { /* ...grab the interrupt vector */
    --         if(!dontSetDMA)
    --           sb_info.DMA=1;                  /* Assume DMA channel 1, because there */
    --         stat=SB_SUCCESS;                  /* is no reliable way to find it.  */
    --       }
    --     }
    --   }
    --   return stat;
    -- }

    -- Exported stuff

    procedure Check_SB_card_presence is
      sbIO: unsigned_16;

      -- A typical case: BLASTER=A220 I5 D1 T4

      BLASTER_var: constant String:=
        To_Upper( GNAT.OS_lib.GetEnv("BLASTER").all );
      b1,b2: Natural;

      procedure Find_Blaster_item( item: Character ) is
      begin
        for s in BLASTER_var'Range loop
          if BLASTER_var(s)= item and s < BLASTER_var'Last then
            b1:= s+1;
            b2:= BLASTER_var'Last;
            for s2 in reverse b1 .. BLASTER_var'Last-1 loop
              if BLASTER_var(s2+1)=' ' then b2:= s2; end if;
            end loop;
            return;
          end if;
        end loop;
        raise SB_BAD_BLASTER;
      end Find_Blaster_item;

    begin
        -- sb_waitInit; ---> DSP.dspreset

      -- if(_go32_dpmi_lock_data(&sb_info,sizeof(sb_info))) return SB_FAILURE;

        if BLASTER_var = "" then
          raise no_blaster_var_cannot_detect;
          -- findSoundBlaster;
        else
          Find_Blaster_item( 'A' );
          sbIO:= unsigned_16'Value("16#" & BLASTER_var( b1..b2 ) & '#');
          case sbIO is
            when 16#210# | 16#220# | 16#230# |
                 16#240# | 16#250# | 16#260# | 16#280# =>
              info.reset:=     sbIO+16#06#;
              info.readData:=  sbIO+16#0A#;
              info.writeData:= sbIO+16#0C#;
              info.dataAvail:= sbIO+16#0E#;

            when others => raise SB_BAD_ADDRESS;
          end case;

          Find_Blaster_item( 'I' );
          info.IRQ:= unsigned_16'Value( BLASTER_var( b1..b2 ) );

          case info.IRQ is
            when 2|3|5|7|10 => null;
            when others => raise SB_BAD_IRQ;
          end case;

          Find_Blaster_item( 'D' );
          info.DMA:= unsigned_8'Value( BLASTER_var( b1..b2 ) );

          case info.DMA is
            when 0|1|3  => null;
            when others => raise SB_BAD_DMA;
          end case;

          begin
            DSP.Hard_Reset; -- Verify address
          exception
            when others => raise SB_BAD_ADDRESS;
          end;

    --       begin
    --         if sb_info.IRQ /= findInterrupt then -- Verify IRQ setting
    --           raise SB_BAD_IRQ;
    --         end if;
    --       exception
    --         when others => raise SB_BAD_IRQ;
    --       end;
        end if;
      exception
        when Data_Error => raise SB_BAD_BLASTER;
     end Check_SB_card_presence;

    --  dontSetDMA : Boolean:= False;

    -- procedure Change_DMA_channel( new_dma: Natural ) is
    -- begin
    --   info.DMA:= unsigned_8( new_dma );
    --   dontSetDMA:= True;
    -- end;

  end SB_Detect;


  package body DSP is

    -- This initializes PIT counter 2 for use in our micro-second delay function.
    procedure sb_waitInit is
      temp: unsigned_8;
    begin
      temp:= Inport( 16#61# );
      temp:= temp and 16#FD#;
      temp:= temp or 1;
      Outport(16#61#, temp);
    end;

    -- This function pauses <number_passed>*0.8381 microseconds.  It simply waits
    -- for <number_passed> ticks to elapse in the PIT, which runs at slightly over
    -- 1 MHz.  The minimum amount of time this function will wait is probably
    -- around 4-6 microseconds, but beyond that it's pretty accurate.
    procedure sb_microWait( number_of_ticks_to_wait: unsigned_16 ) is
      elapsed:  unsigned_16;
      failsafe: unsigned_32;
      begin
       Outport( 16#43#, 16#B0# );
       Outport( 16#42#, 16#FF# );
       Outport( 16#42#, 16#FF# );

      -- C: Sometimes this timer doesn't seem to work, and our program hangs!
      failsafe:= unsigned_32(number_of_ticks_to_wait) * 10000;

      loop
        Outport( 16#43#, 16#80#);
        elapsed:= unsigned_16(Inport( 16#42# ));
        elapsed:= elapsed + unsigned_16(Inport( 16#42# )) * 256;
        elapsed:= not elapsed; -- elapsed=~elapsed; ???
        exit when elapsed >=
          number_of_ticks_to_wait; -- and failsafe; --(C??)
        failsafe:= failsafe - 1;
      end loop;

    end sb_microWait;

    procedure Write( v: unsigned_8 ) is
    begin
      while (Inport( info.writeData ) and 128) /= 0 loop null; end loop;
      Outport( info.writeData, v );
    end Write;

    function Read return unsigned_8 is
    begin
      while (Inport( info.dataAvail ) and 128) = 0 loop null; end loop;
      return Inport( info.readData );
    end Read;

    procedure Hard_Reset is
    begin
      sb_waitInit; -- de: sb_detect (usage unique...)

      Outport( info.reset, 1);

      sb_microWait(4);

      Outport( info.reset, 0);

      for a in 1..1000 loop
        if (Inport( info.dataAvail ) and 128) /= 0 then
          for b in 1..1000 loop
            if Inport( info.readData ) = 16#AA# then
              DSP.Write( 16#E1# );
              info.dspVersion:= unsigned_16( DSP.Read );
              info.dspVersion:= info.dspVersion * 256 +
                                     unsigned_16( DSP.Read );
              if info.dspVersion >= 16#040C# then
                info.model:= SB_AWE_32;
              elsif info.dspVersion >= 16#0400# then
                info.model:= SB_16;
              elsif info.dspVersion >= 16#0300# then
                info.model:= SB_Pro;
              elsif info.dspVersion >  16#0202# then
                info.model:= SB_2_HiSpeed;
              elsif info.dspVersion >= 16#0201# then
                info.model:= SB_2;
              elsif info.dspVersion  = 16#0200# then
                info.model:= SB_1_5;
              else
                info.model:= SB_1;
              end if;
              return; -- success!
            end if;
          end loop;
          exit; -- inner loop failed
        end if;
      end loop;
      raise SB_FAILURE;
    end Hard_Reset;

    procedure Set_time_constant( freq_for_DMA_transfers: Positive ) is
      tc: unsigned_8;
    begin
      DSP.Write( 16#40# );

      tc:= 0 - unsigned_8(1000000 / freq_for_DMA_transfers);
      DSP.Write(tc);
    end;

    procedure Set_high_speed_time_const( freq: Positive ) is
      tc: unsigned_16;
    begin
      DSP.Write( 16#40# );

      tc:= 0 - unsigned_16(256000000 / freq);
      tc:= tc / 256;
      DSP.Write( unsigned_8(tc) );
    end;

    procedure Start_DMA_transfer_Auto_Init( Size_of_tr_buffer_m_1: unsigned_16 ) is
    begin
      DSP.Write( 16#48# );
      DSP.Write( unsigned_8(Size_of_tr_buffer_m_1 mod 256) );
      DSP.Write( unsigned_8(Size_of_tr_buffer_m_1  /  256) );

      DSP.Write( 16#1C# );
    end Start_DMA_transfer_Auto_Init;

    procedure Start_high_speed_DMA_transfer_Auto_Init(
                 Size_of_tr_buffer_m_1: unsigned_16 ) is
      begin
        DSP.Write( 16#48# );
        DSP.Write( unsigned_8(Size_of_tr_buffer_m_1 mod 256) );
        DSP.Write( unsigned_8(Size_of_tr_buffer_m_1  /  256) );
        DSP.Write( 16#90# );
      end;

    -- static int topOfFunctions(void) { }

    procedure Start_DMA_transfer_Single_Cycle( Size_of_tr_buffer_m_1: unsigned_16 ) is
    begin
       DSP.Write( 16#14# );
       DSP.Write( unsigned_8(Size_of_tr_buffer_m_1 mod 256) );
       DSP.Write( unsigned_8(Size_of_tr_buffer_m_1  /  256) );
    end;

  -- static int bottomOfFunctions(void) { }

  -- int sb_dsp_module_lock_your_memory(void) {
  --   return !(_go32_dpmi_lock_code(topOfFunctions,
  --   ((char *)bottomOfFunctions-(char *)topOfFunctions)));
  -- }

  end DSP;

  ------------------------------------
  -- The contents of driver package --
  ------------------------------------

  sb_dmaBufferLinearAddress : array( 0..1 ) of Unsigned_32;
  sb_miniDMABufferAddress   : array( 0..1 ) of Unsigned_32;

  sb_currentBlock : Integer;

  mixerDefault            : Unsigned_8;
  pic1Default, pic2Default: Unsigned_8;
  dmaBufferSelector       : Unsigned_16;
  End_of_DMA_Intr_Vec     : Unsigned_8;

  procedure Interrupt_handler is
    dummy: Unsigned_8;
    addr : Unsigned_32;
  begin
    if info.model >= SB_Pro then -- Use STEREO

      left_buffer  := (others => 128); -- neutral level
      right_buffer := (others => 128); -- neutral level
      SB_Digi.Mix_stereo_samples;

      addr:= sb_dmaBufferLinearAddress(sb_currentBlock);
      Farsetsel( Current_Info.Selector_For_Linear_Memory );

      for a in left_buffer'Range loop
        if left_buffer(a) < 0 then
          Farnspokeb(addr, 0 );
        elsif left_buffer(a) > 255 then
          Farnspokeb(addr, 255 );
        else
          Farnspokeb(addr, Unsigned_8(left_buffer(a)) );
        end if;
        addr:= addr + 2;
      end loop;

      addr:= sb_dmaBufferLinearAddress(sb_currentBlock) + 1;

      for a in right_buffer'Range loop
        if right_buffer(a) < 0 then
          Farnspokeb(addr, 0 );
        elsif right_buffer(a) > 255 then
          Farnspokeb(addr, 255 );
        else
          Farnspokeb(addr, Unsigned_8(right_buffer(a)) );
        end if;
        addr:= addr + 2;
      end loop;

    else --  Use MONO (Blech!)

      mixing_buffer:= (others => 128); -- neutral level
      SB_Digi.Mix_mono_samples;

      addr:= sb_miniDMABufferAddress(sb_currentBlock);
      Farsetsel( Current_Info.Selector_For_Linear_Memory );

      for a in mixing_buffer'Range loop
        if mixing_buffer(a) < 0 then
          Farnspokeb(addr, 0 );
        elsif mixing_buffer(a) > 255 then
          Farnspokeb(addr, 255 );
        else
          Farnspokeb(addr, Unsigned_8(mixing_buffer(a)) );
        end if;
        addr:= addr + 2;
      end loop;

    end if;

    sb_currentBlock:= 1-sb_currentBlock;

    if info.model = SB_1 then
      DMA.Read_8bit_Single_Cycle(sb_miniDMABufferAddress(sb_currentBlock),
                                 Half_Block-1);
      DSP.Start_DMA_transfer_Single_Cycle( Half_Block-1 );
    end if;

    dummy:= Inport(info.dataAvail);

    Outport(16#20#,16#20#);
    Outport(16#A0#,16#20#);

  end Interrupt_handler;

  -- Allocate some DOS memory that doesn't cross a PAGE boundary (for the DMA
  -- buffer.)

  procedure allocateDosMem(bytes: Unsigned_16; dosSeg, dosSel: out Unsigned_16) is
    firstPage, lastPage: Unsigned_16;
    linearAddress: Unsigned_32;
    Segs, Sels: array( 0 .. 15 ) of Unsigned_16;
    paragraphs: constant Unsigned_16:= (bytes+15) / 16;
    currentTry: Natural:= 0;

  begin
    loop
      if currentTry > 15 then raise Unable_to_allocate_DOS_memory; end if;
      Segs(currentTry):= DOS_Alloc(paragraphs, Sels(currentTry)'Address);

      if Segs(currentTry)=-1 then raise Unable_to_allocate_DOS_memory; end if;

      linearAddress:= Unsigned_32(Segs(currentTry)) * 16;

      firstPage:= Unsigned_16( linearAddress / 65536 );
      lastPage:=  Unsigned_16( (linearAddress+Unsigned_32(bytes)-1) / 65536 );

      exit when firstPage = lastPage;

      currentTry:= currentTry + 1;
    end loop;

    dosSeg:= Segs(currentTry);
    dosSel:= Sels(currentTry);

    for i in 0 .. currentTry-1 loop DOS_free( Sels(currentTry) ); end loop;

  end allocateDosMem;

  installed: Boolean:= False;

  procedure Install( max_freq: Natural; success: out Boolean ) is
    picMask: Unsigned_8;
    fq: Natural:= max_freq;
  begin
    success:= False;
    if installed then raise Already_installed; end if;

    SB_Detect.Check_SB_card_presence;
    begin
      allocateDosMem( DMA_Buf_size,
                      info.dmaBufferSegment, dmaBufferSelector );
    exception
      when Unable_to_allocate_DOS_memory => raise SB_FAILURE;
    end;

    sb_dmaBufferLinearAddress(0):= Unsigned_32(info.dmaBufferSegment) * 16;
    sb_dmaBufferLinearAddress(1):= sb_dmaBufferLinearAddress(0) +
                                    Block_size;
    sb_miniDMABufferAddress(0):= sb_dmaBufferLinearAddress(0);
    sb_miniDMABufferAddress(1):= sb_dmaBufferLinearAddress(0) + Half_Block;

    pic1Default:= Inport( 16#21# );
    pic2Default:= Inport( 16#A1# );

    if info.IRQ < 8 then
      End_of_DMA_Intr_Vec:= Unsigned_8(info.IRQ) + 16#08#;
      picMask:= shift_left( 1, Integer(info.IRQ) );
      picMask:= not picMask;
      Outport(16#21#, pic1Default and picMask); -- Enable PIC-1's IRQ
    else
      End_of_DMA_Intr_Vec:= Unsigned_8(info.IRQ) + 16#68#;
      picMask:= shift_left( 1, Integer(info.IRQ-8) );
      picMask:= not picMask;
      Outport(16#21#,pic1Default and 16#FB#);  -- Enable IRQ2
      Outport(16#A1#,pic2Default and picMask); -- As well as PIC-2's IRQ
    end if;

--     if((_go32_dpmi_lock_code(topOfFunctions,((char *)bottomOfFunctions-(char *)topOfFunctions))) ||
--        (_go32_dpmi_lock_data(&wrapper,sizeof(_go32_dpmi_seginfo))) ||
--        (_go32_dpmi_lock_data(sb_dmaBufferLinearAddress,2*sizeof(DWORD))) ||
--        (_go32_dpmi_lock_data(&sb_dmaBufferLinearAddress,sizeof(DWORD *))) ||
--        (_go32_dpmi_lock_data(sb_miniDMABufferAddress,2*sizeof(DWORD))) ||
--        (_go32_dpmi_lock_data(&sb_miniDMABufferAddress,sizeof(DWORD *))) ||
--        (_go32_dpmi_lock_data(&sb_currentBlock,sizeof(int))) ||
--        (_go32_dpmi_lock_data(&fq,sizeof(int))) ||
--        (!sb_dma_module_lock_your_memory()) ||
--        (!sb_dsp_module_lock_your_memory()) ||

--        (!sb_digi_module_lock_your_memory())) {
--       strcpy(sb_driver_error,"Unable to lock appropriate memory.");
--       return SB_FAILURE;
--     }

    sb_currentBlock:= 0;

    if fq < 5000 then fq:=5000; end if;

    case info.model is
      when SB_2_HiSpeed | SB_16..SB_AWE_32 =>
        if fq > 45454 then fq:= 45454; end if;
      when SB_Pro =>
        if fq > 22727 then fq:= 22727; end if;
      when others =>
        if fq > 22222 then fq:= 22222; end if;
    end case;

    Dos_Interrupts.Set_Interrupt( End_of_DMA_Intr_Vec,
                                  Interrupt_handler'Address );

    installed:= True; -- At this stage we *must* consider the driver as inst'd

    case info.model is
      when SB_16..SB_AWE_32 => DSP.Set_high_speed_time_const(fq);
      when SB_Pro           => DSP.Set_high_speed_time_const(fq*2);
      when others           => DSP.Set_time_constant(fq);
    end case;

    SB_digi.initialize;

    DSP.Write( 16#D1# ); -- Turn the speaker on

    case info.model is
     when SB_16..SB_AWE_32 =>

      DMA.Read_8bit_AutoInit(sb_dmaBufferLinearAddress(0), DMA_Buf_size-1);
      DSP.Write( 16#C4# );
      DSP.Write( 16#20# );
      declare
        lo: constant:= (Block_size-1) mod 256;
        hi: constant:= (Block_size-1)  /  256;
      begin
        DSP.Write(lo);
        DSP.Write(hi);
      end;

     when SB_Pro =>

      Outport(info.reset-2,16#0E#);
      mixerDefault:= Inport(info.reset-1);
      Outport(info.reset-2,16#0E#);
      Outport(info.reset-1,mixerDefault or 16#22#);
      DMA.Read_8bit_AutoInit(sb_dmaBufferLinearAddress(0), DMA_Buf_size-1);
      DSP.Start_high_speed_DMA_transfer_Auto_Init( Block_size-1 );

     when SB_2_HiSpeed =>
      DMA.Read_8bit_AutoInit(sb_dmaBufferLinearAddress(0), DMA_Buf_size-1 );
      DSP.Start_high_speed_DMA_transfer_Auto_Init( Block_size-1 );

     when SB_1_5 | SB_2 =>
      DMA.Read_8bit_AutoInit(sb_miniDMABufferAddress(0), Block_size-1);
      DSP.Start_DMA_transfer_Auto_Init( Half_Block-1 );

     when SB_1 =>
      DMA.Read_8bit_Single_Cycle(sb_miniDMABufferAddress(0), Half_Block-1 );
      DSP.Start_DMA_transfer_Single_Cycle( Half_Block-1 );
    end case;

    sample_frequency:= fq;
    success:= True;

  exception
    when SB_Failure => success:= False; -- SB not detected or resetted
  end Install;

  procedure Uninstall is
  begin
    if not installed then return; end if;

    if info.model < SB_2_HiSpeed then
      if info.model= SB_1_5 then
        DSP.Write( 16#DA# );
      end if;
      DSP.Write( 16#D0# );    -- Stop any DMA transfers currently going on
    end if;
    DSP.Hard_Reset;
    DSP.Write( 16#D3# );      -- and turn its speaker-output off

    Dos_Interrupts.Remove_Interrupt( End_of_DMA_Intr_Vec );
    installed:= False;

    Outport(16#21#,pic1Default);
    Outport(16#A1#,pic2Default);

    if info.model = SB_Pro then
      Outport(info.reset-2,16#0E#);
      Outport(info.reset-1,mixerDefault);
    end if;

    DOS_Free( dmaBufferSelector );
  end Uninstall;

end SB_Driver;

