------------------------------------------------------------------------------
--  File:            SB_Digi.ads       (possibly extracted from DOS_PAQS.ZIP)
--  Description:     Sound Blaster package,
--                     Software part (mixing, queuing, I/O for samples)
--  Date/version:    14-Sep-2003; 13-Jan-2001 ; 20-Apr-2000
--  Author:          Gautier de Montmollin
--  References:      1) The Sound-Blaster Library for DJGPP v2, v.0.5, by
--                      Joel H. Hunter (based on Varmint's Audio Tools by
--                      Eric Jorgensen), 1995
------------------------------------------------------------------------------
-- 13-Jan-2001: removed sb_* in names (it's clear from where it comes)

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;                        use Interfaces;

package SB_Digi is

  type Digi_data is array( Integer range <> ) of Integer_8;  -- signed data
  type Byte_data is array( Integer range <> ) of Unsigned_8; -- unsigned data

  subtype Volume_type is Natural range 0..63;
  volume_span: constant:= 64;
  normal_volume: constant:= 32;

  type Sample( length_min_1: Integer ) is record
    data         : Digi_data( 0 .. length_min_1 );
    stereo       : Boolean:= False;
    bits         : Integer:= 64;
    left_volume  : Volume_type:= normal_volume;
    right_volume : Volume_type:= normal_volume;
    name         : String( 1 .. 12 ):= "--Untitled--";
    -- original frequency to be added here, to make freq. conv. easier [!!!]
  end record;

  type p_Sample is access Sample;

  -- Formats to pass to the sb_load_sample procedure
  type digi_formats is ( SB_RAW, SB_WAV, SB_VOC );

  -- Load a digital sound from a data stream
  function Load_sample(s: Stream_Access; f:digi_formats;
                       guess_raw_size: Count:= 0 ) return Sample;

  -- Load a digital sound from a file
  function Load_sample(fname: String; f:digi_formats ) return Sample;

  VOC_unsupported, Invalid_WAV_file : exception;


  function Convert_frequency(
             s: Sample;
             freq_from, freq_to: Integer ) return Sample;

  procedure Free_sample is
    new Ada.Unchecked_Deallocation(Sample, p_Sample);

  procedure Initialize;

  procedure Queue_a_sample( s: p_Sample );
  procedure Mix_a_sample( s: p_Sample );

  function Remaining_samples_in_queue return Natural;
  function Remaining_samples_mixed return Natural;

  procedure Mix_stereo_samples;
  procedure Mix_mono_samples;

  -- Maximum samples that can be mixed or queued at a time.
  SB_MAX_MIX   : constant:= 16;
  SB_MAX_QUEUE : constant:= 16;

  SB_BAD_POINTER,
  SB_BUSY,
  SB_BAD_FILE,
  SB_INTERNAL: exception;

end SB_Digi;
