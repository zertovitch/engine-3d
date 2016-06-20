------------------------------------------------------------------------------
--  File:            Eng3DOpt.ads
--  Description:     Basic 3D engine : options
--  Date / Version:  29-Jul-2001 ; 15-Jul-2001
------------------------------------------------------------------------------

package Engine_3D.Options is

  -- * Scanline Profiling: mode for adjusting some constants;
  --   creates scanline.log and puts there the usage frequency
  --   of the scanlines methods.
  scanline_profiling: constant Boolean:= True;

  -- * Show hidden faces (hidden <=> projected normal is positive) ?
  --   Normally it is useless to do so;
  --   a well constructed 3D world won't show any difference
  --   but will result in a much larger drawing time and
  --   an *infinite recursion* in mutually connected portals !
  show_hidden_faces: constant Boolean:= False;

  -- * Draw lines to signal where portals are
  show_portals: constant Boolean:= False;

  -- * Check, at object's startup, for vertices having a zero average
  --   of adjacent faces' normals: problematic for Gouraud shading
  check_zero_averaged_normals: constant Boolean:= False;

  -- * Depth range for drawing portals.
       subtype Full_portal_draw_depth_range is Natural range 0..1000;
       subtype Only_level_1 is Natural range 1..1;

    -- Here, the choice:
  subtype Portal_draw_depth_range is Full_portal_draw_depth_range;

  -- * Subpixel correction
  subpixel: constant Boolean:= True;

end Engine_3D.Options;

