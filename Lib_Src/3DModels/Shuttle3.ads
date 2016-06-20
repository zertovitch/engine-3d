------------------------------------------------------------------------------
--  File:            Shuttle3.ads
--  Description:     Space shuttle 3D model.
--                   Copyright (c) Gautier de Montmollin 1999-2000
--  Date / Version:  3-Sep-2000 / 17.VIII.1999
------------------------------------------------------------------------------
--  3-Sep-2000: palette optimized

with Engine_3D;

package Shuttle3 is

  Shuttle3_obj: Engine_3D.p_Object_3D;

  procedure Init;

  -- Textures: face numbers for the parts with labels
  -- Wings: scale 1.46x1
  wing_right_a: constant:= 145; -- 2 triangles, same texture
  wing_right_b: constant:= 147;
  wing_left_a:  constant:= 146; -- 2 triangles, same texture
  wing_left_b:  constant:= 148;

  side_right: constant:= 1; -- scale 6x1
  side_left:  constant:= 2;
  top_hubl_right: constant:= 35; -- triangle, pict. on low. left
  top_hubl_left:  constant:= 36; -- triangle, pict. on up left
  side_hubl_right: constant:=  9; -- scale 20x1
  side_hubl_left:  constant:= 10;

end Shuttle3;
