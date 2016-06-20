-----------------------------------------------------------------------------
--  File: sb_digi.adb; see specification (sb_digi.ads)
-----------------------------------------------------------------------------
with SB_Driver;

package body SB_Digi is

  -- A multiplication table to speed up performance-critical inner loops.
  mul_table: array( volume_type, integer_8 ) of integer_16;

  type sampleTracker is record  -- Just a little structure to keep track of
    sample       : p_Sample;    -- how much of a sample I have played.
    elapsedBytes : Integer;
    bytesLeft    : Integer;
  end record;

  sb_queue      : array( 1 .. SB_MAX_QUEUE ) of p_Sample;
  sb_numInQueue : Natural:= 0;

  sb_mix      : array( 1 .. SB_MAX_MIX ) of sampleTracker;
  sb_numInMix : Natural:= 0;

  function Remaining_samples_in_queue return Natural is
    begin return sb_numInQueue; end;

  function Remaining_samples_mixed return Natural is
    begin return sb_numInMix; end;

  procedure Mix_a_sample( s: p_Sample ) is
    nmix: Integer:= sb_numInMix;
    -- We copy it fast before it is decreased by the interrupt !
  begin
    if s=null then raise SB_BAD_POINTER; end if;

    -- ASM("CLI");
    -- Bug in the C code: sb_numInMix and sb_mix could be changed during
    -- this procedure by the interruption !!

    if nmix < 0 then raise SB_INTERNAL; end if;
    if nmix = sb_mix'Last then
      for a in sb_mix'First .. sb_mix'Last-1 loop
        sb_mix(a):= sb_mix(a+1);
      end loop;
      nmix:= sb_mix'Last-1;
    end if;
    nmix:= nmix + 1;
    if nmix < 1 then raise SB_INTERNAL; end if;

    -- Ada.Text_IO.put( integer'image(nmix) );

    sb_mix(nmix).elapsedBytes:= 0;
    sb_mix(nmix).bytesLeft   := s.length_min_1 + 1;
    sb_mix(nmix).sample      := s;

    -- ASM("STI");
    sb_numInMix:= nmix;
--   exception
--     when others => Ada.Text_IO.put("?????? "& integer'image(nmix) );
--     raise;
  end Mix_a_sample;

  generic with procedure mix_it( a, b_min, b_max: Integer);
  procedure mix_samples;

  procedure mix_samples is
    a: Integer;
  begin
    a:= sb_mix'First;
    loop
      exit when a > sb_numInMix; -- i.e. immediately when sb_numInMix = 0

      if sb_mix(a).bytesLeft > SB_Driver.Half_block then
        mix_it( a, 1, SB_Driver.Half_block );
        sb_mix(a).bytesLeft:= sb_mix(a).bytesLeft - SB_Driver.Half_block;

      else -- less than Half_block remaining

        mix_it( a, 1, sb_mix(a).bytesLeft );

        if sb_mix(a).sample = sb_queue(1) then

          if sb_numInQueue > 1 then
            Mix_a_sample( sb_queue(2) );
            sb_numInQueue:= sb_numInQueue -1;
            for b in 1 .. sb_numInQueue loop
              sb_queue(b):= sb_queue(b+1);
            end loop;

            mix_it( sb_numInMix, sb_mix(a).bytesLeft+1, SB_Driver.Half_block );

            sb_mix(sb_numInMix).bytesLeft:= sb_mix(sb_numInMix).bytesLeft -
              sb_mix(sb_numInMix).elapsedBytes;

          elsif sb_numInQueue > 0 then
            sb_numInQueue:= sb_numInQueue-1;
          end if;
        end if; -- de-queue

        sb_numInMix:= sb_numInMix-1;   -- sample #a disappears
        for b in a .. sb_numInMix loop
          sb_mix(b):= sb_mix(b+1);
        end loop;
        a:= a-1;
      end if;
      a:= a+1;
    end loop;
  end mix_samples;

  procedure mix_it_mono( a, b_min, b_max: Integer) is
    elap: Integer renames sb_mix(a).elapsedBytes;
    temp: constant Integer:=
                    ( sb_mix(a).sample.left_volume +
                      sb_mix(a).sample.right_volume ) / 2;
  begin
    for b in b_min..b_max loop
      exit when elap > sb_mix(a).sample.data'Last;

      SB_Driver.mixing_buffer(b):= SB_Driver.mixing_buffer(b) +
        mul_table(temp, sb_mix(a).sample.data(elap)) / 32;
      elap:= elap + 1;
    end loop;
  end mix_it_mono;

  procedure i_mix_mono_samples   is new mix_samples( mix_it_mono );
  procedure Mix_mono_samples renames i_mix_mono_samples;

  procedure mix_it_stereo( a, b_min, b_max: Integer) is
    elap: Integer renames sb_mix(a).elapsedBytes;
  begin
    for b in b_min..b_max loop
      exit when elap > sb_mix(a).sample.data'Last;

      if sb_mix(a).sample.left_volume > 0 then
        SB_Driver.left_buffer(b):= SB_Driver.left_buffer(b) +
          mul_table(
            sb_mix(a).sample.left_volume,
            sb_mix(a).sample.data(elap)
          ) / normal_volume;
      end if;

      if sb_mix(a).sample.right_volume > 0 then
        SB_Driver.right_buffer(b):= SB_Driver.right_buffer(b) +
          mul_table(
            sb_mix(a).sample.right_volume,
            sb_mix(a).sample.data(elap)
          ) / normal_volume;
      end if;

      elap:= elap + 1;
    end loop; -- b
  end mix_it_stereo;

  procedure i_mix_stereo_samples is new mix_samples( mix_it_stereo );
  procedure Mix_stereo_samples renames i_mix_stereo_samples;

  procedure Queue_a_sample( s: p_Sample ) is
  begin
    if s=null then raise SB_BAD_POINTER; end if;
    if sb_numInQueue >= SB_MAX_QUEUE then raise SB_BUSY; end if;

    if sb_numInQueue=0 then  -- If the queue is empty right now, just
      Mix_a_sample(s);       -- start playing this one right away.
    end if;
    sb_numInQueue:= sb_numInQueue + 1;
    sb_queue(sb_numInQueue):= s;
  end Queue_a_sample;

  function Convert_frequency(
             s: Sample;
             freq_from, freq_to: Integer ) return Sample is

    ratio: constant Long_Float:= Long_Float(freq_from) / Long_Float(freq_to);
    ss: Sample( length_min_1=> Integer(Long_Float(s.length_min_1) / ratio) );

  begin
    for a in ss.data'Range loop
      ss.data(a):= s.data( Integer(Long_Float(a) * ratio) );
    end loop;

    ss.stereo:= s.stereo;
    ss.bits:=   s.bits;
    ss.left_volume:=  s.left_volume;
    ss.right_volume:= s.right_volume;
    ss.name:=         s.name;
    return ss;
  end Convert_frequency;

  procedure Initialize is
  begin
    for a in mul_table'Range(1) loop
      for b in mul_table'Range(2) loop
        mul_table(a,b):= integer_16(a) * integer_16(b);
      end loop;
    end loop;

    sb_numInQueue:= 0;
    sb_numInMix:=   0;

  end Initialize;

-- int sb_digi_module_lock_your_memory(void) {
--   if((_go32_dpmi_lock_data(&sb_mixing_buffer,sizeof(int *))) ||
--      (_go32_dpmi_lock_data(&sb_left_buffer,sizeof(int *))) ||
--      (_go32_dpmi_lock_data(&sb_right_buffer,sizeof(int *))) ||
--      (_go32_dpmi_lock_data(sb_queue,_SB_MAX_QUEUE*sizeof(sb_sample *))) ||
--      (_go32_dpmi_lock_data(&sb_queue,sizeof(sb_sample **))) ||
--      (_go32_dpmi_lock_data(sb_mix,_SB_MAX_MIX*sizeof(sampleTracker))) ||
--      (_go32_dpmi_lock_data(&sb_mix,sizeof(sampleTracker *))) ||
--      (_go32_dpmi_lock_data(&sb_numInQueue,sizeof(int))) ||
--      (_go32_dpmi_lock_data(&sb_numInMix,sizeof(int))) ||
--      (_go32_dpmi_lock_data(mul_table,256*64*sizeof(int))) ||
--      (_go32_dpmi_lock_data(&mul_table,sizeof(int *))) ||
--      (_go32_dpmi_lock_code(topOfFunctions,((char *)bottomOfFunctions-(char *)topOfFunctions))))
--     return 0;
--   return 1;
-- }
--

  function Load_Wave(s: Stream_Access) return Sample is
    rlen,flen, sample_len: Unsigned_32;
    s_per_sec,b_per_sec,num_channels,tag: unsigned_16;
    riffid,waveid,fmtid,dataid: String( 1..4 );
    temp: Unsigned_8;

  begin
    String'Read( s, riffid );
    Unsigned_32'Read( s, rlen );
    String'Read( s, waveid );
    if riffid /= "RIFF" or waveid/="WAVE" then
      raise Invalid_WAV_file;
    end if;

    String'Read( s, fmtid );
    Unsigned_32'Read( s, flen );
    if flen>240 then flen:=240; end if;

    Unsigned_16'Read( s, tag );
    Unsigned_16'Read( s, num_channels );
    Unsigned_16'Read( s, s_per_sec );
    Unsigned_16'Read( s, b_per_sec );

    for i in 1 .. Natural(flen)-8 loop unsigned_8'Read( s, temp ); end loop;

    String'Read( s, dataid );
    Unsigned_32'Read( s, sample_len );

    declare
      ss:    Sample( length_min_1=> Natural( sample_len ) - 1 );
      sound: byte_data( ss.data'Range );
    begin
      byte_data'Read( S, sound );
      -- Convert sample to SIGNED for mixing algorithm:
      for i in ss.data'Range loop
        ss.data(i):= integer_8( Integer(sound(i)) - 128);
      end loop;
      return ss;
    end;

  end Load_Wave;

  function Load_Raw(s: Stream_Access; size: Natural) return Sample is
    ss   : Sample( length_min_1=> size - 1 );
    sound: byte_data( ss.data'Range );
  begin
    byte_data'Read( s, sound );
    -- Convert sample to SIGNED for mixing algorithm:
    for i in ss.data'Range loop
        ss.data(i):= integer_8( Integer(sound(i)) - 128);
    end loop;
    return ss;
  end Load_Raw;

  function Load_sample(s: Stream_Access; f:digi_formats;
                       guess_raw_size: Count:= 0 ) return Sample is
  begin
    case f is
      when SB_RAW => return Load_Raw(s, Natural(guess_raw_size) );
      when SB_WAV => return Load_Wave(s);
      when SB_VOC => raise VOC_unsupported;
    end case;
  end Load_sample;

  function Load_sample(fname: String; f:digi_formats ) return Sample is
    fi: File_Type;
    fl: Natural:= fname'Length;
  begin
    Open(fi, in_file, fname);
    declare
      tmp_sample: Sample:= Load_sample( stream(fi), f, Size(fi) );
    begin
      Close(fi);
      -- We have a file name, so we give it to sample!
      if fl > tmp_sample.name'Length then -- avoid a too long one
        fl:= tmp_sample.name'Length;
      end if;
      tmp_sample.name:= (others=> ' '); -- first, blank the string
      tmp_sample.name( 1..fl ):= fname( fname'Last-fl+1..fname'Last ); -- tail
      return tmp_sample;
    end;
  end Load_sample;

begin
  Initialize;
end SB_Digi;
