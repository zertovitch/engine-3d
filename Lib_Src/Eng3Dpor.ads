------------------------------------------------------------------------------
--  File:            Eng3DPor.ads
--  Description:     Basic 3D engine - functions for portal rendering
--  Date / Version:  25-Jul-2001 ; 17-Jul-2001 ; 9-Dec-2000 ; 6-Aug-2000
--  Author:          Gautier de Montmollin
--
--  Copyright (c) Gautier de Montmollin 2000 .. 2001
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

package Engine_3D.Portals is

  type Rectangle is record X1,Y1,X2,Y2: Integer; end record;

  subtype View_frustum is Rectangle;

  -- [a] Cheap but fast portal method with rectangles.

  -- Inconveniants of using only this:
  --  1. Normally, the bounding rectangle is larger than a face
  --  2. Even with sorting, you see this problem
  --  3. With an exact portal clipping, you can render connected
  --     convex polygons without sorting at all :-)

  procedure Intersect (A,B: Rectangle; C: out Rectangle; non_empty: out Boolean);
  procedure Find_bounding_box(o: Object_3D; face: Natural; b: out Rectangle);
  procedure Draw_boundary( buffer: out Screen_Buffer; clip: Rectangle );

  -- [b] Exact portal : the visible area is convex and determined by
  -- xmin(y) .. xmax(y), just for scanlines. There is also ymin(x) .. ymax(x)
  -- for facilitating vertical scanlines.

  -- < not done yet! >

end Engine_3D.Portals;
