------------------------------------------------------------------------------
--  File:            SBDriver.ads      (possibly extracted from DOS_PAQS.ZIP)
--  Description:     Sound Blaster package,
--                     Hardware part (protected mode driver)
--  Date/version:    13-Jan-2001 ; 4-Nov-2000 ; 20-Apr-2000
--  Author:          Gautier de Montmollin
--  References:
--    1) The Sound-Blaster Library for DJGPP v2, v.0.5, by Joel H. Hunter,
--        based on Varmint's Audio Tools by Eric Jorgensen), 1995
--    2) Creative Labs documentation,
--        ftp://ftp.creaf.com/pub/creative/devinfo/ctsbhwpg.exe
--        Sound Blaster Series Hardware Programming Guide, 1996 (PDF format)
------------------------------------------------------------------------------
-- 13-Jan-2001: removed sb_* in names (it's clear from where it comes)

with Interfaces;                        use Interfaces;

package SB_Driver is

  -- Size of the DMA transfer buffer. Smaller sizes give better resolution in
  -- sample mixing, but take more CPU resources.

  DMA_Buf_size: constant:= 1024;  -- Orig: 96; Allegro: ~256..1024
  Block_size  : constant:= DMA_Buf_size / 2;
  Half_block  : constant:= Block_size / 2;
  -- The mixing buffers.
  Mixing_buffer: array( 1..Block_size ) of integer_16;
  Left_buffer  : array( 1..Half_block ) of integer_16;
  Right_buffer : array( 1..Half_block ) of integer_16;
  -- NB: memory must be locked as RAM!

  -- Archeological classification of Sound Blaster models
  --   | Main source: Joel H. Hunter C sources for SB DJGPP lib.

  type Model_choice is
                ( SB_1,            --  <  16#0100#
                  SB_1_5,          --   = 16#0200#
                  SB_2,            --  >= 16#0201#
                  SB_2_HiSpeed,    --  >  16#0202# -- maybe doesn't exist
                  SB_Pro,          --  >= 16#0300#
                  SB_16,           --  >= 16#0400#
                  SB_AWE_32        --  >= 16#040C#
                );

  -- About enumeration names: for last model read: "... or later";
  -- otherwise: "... or later, till next model" !

  type t_sb_info is record
    reset           : unsigned_16;
    readData        : unsigned_16;
    writeData       : unsigned_16;
    dataAvail       : unsigned_16;
    IRQ             : unsigned_16;
    DMA             : unsigned_8;
    dspVersion      : unsigned_16;
    model           : Model_choice;
    dmaBufferSegment: unsigned_16;
  end record;

  info: t_sb_info;

  sample_frequency: Natural;

  -- SB driver interrupt installation. Needs uninstall
  -- Nov-2000: _SB_driver removed in names: simply write SB_driver.Install

  procedure Install( max_freq: Natural; success: out Boolean );
  procedure Uninstall;
  -- NB: raises an exception if anything have gone wrong.

  SB_FAILURE,
  SB_BAD_BLASTER,
  SB_BAD_ADDRESS,
  SB_BAD_IRQ,
  SB_BAD_DMA: exception;

  Unable_to_allocate_DOS_memory, Already_installed : exception;

end SB_Driver;
