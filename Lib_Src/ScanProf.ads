-- GdM 5-oct-2000: make a statistic about the texture mapping modes chosen

with Engine_3D;

package Scanline_profiler is
  procedure Initialize;
  procedure Add_one_count( m: Engine_3D.texture_mapping_mode );
  procedure Finalize;
end Scanline_profiler;
