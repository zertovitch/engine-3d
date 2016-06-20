--  Meteor 3D model. Copyright (c) Gautier de Montmollin 1999
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use the herein contained 3D model for any purpose,
--  provided this copyright note remains attached and unmodified.

with SVGA;                              use SVGA;
with SVGA.Effects;                      use SVGA.Effects;

package body Meteor is

  procedure Init(meteor_texture: Engine_3D.p_p_Texture_map) is

    use Engine_3D;

    xprop,yprop,zprop: Float;

    int_min: constant intensity:= (90 * intensity'First) / 100;
    int_max: constant intensity:= (70 * intensity'Last)  / 100;
    cmin, cmax: color_type;

  -- V. 1999 en quadrilateres, peu plats

  --    meteor01_smmts: array( 1..22, 1..4) of positive :=
  --      ( (1, 2, 3, 4), ( 1, 4, 5, 6),                   -- 1
  --        (7, 8, 2, 1), ( 8, 9, 3, 2), ( 9,10, 4, 3),
  --          (10,11, 5, 4), (11,12, 6, 5), (12, 7, 1, 6), -- 2
  --        (13,14, 8, 7), (14,15, 9, 8), (15,16,10, 9),
  --          (16,17,11,10), (17,18,12,11), (18,13, 7,12), -- 3
  --        (19,20,14,13), (20,21,15,14), (21,22,16,15),
  --          (22,23,17,16), (23,24,18,17), (24,19,13,18), -- 4
  --        (19,24,23,22), (22,21,20,19) );                -- 5

  -- V. 2002: en triangles

    meteor01_smmts: constant array( 1..44, 1..4) of Natural :=
      (
        ( 1, 2, 3, 0),
        ( 1, 4, 5, 0), -- 1
        ( 7, 8, 2, 0),
        ( 8, 9, 3, 0),
        ( 9,10, 4, 0),
        (10,11, 5, 0),
        (11,12, 6, 0),
        (12, 7, 1, 0), -- 2
        (13,14, 8, 0),
        (14,15, 9, 0),
        (15,16,10, 0),
        (16,17,11, 0),
        (17,18,12, 0),
        (18,13, 7, 0), -- 3
        (19,20,14, 0),
        (20,21,15, 0),
        (21,22,16, 0),
        (22,23,17, 0),
        (23,24,18, 0),
        (24,19,13, 0), -- 4
        (19,24,23, 0),
        (22,21,20, 0), -- 5

        ( 1, 0, 3, 4),
        ( 1, 0, 5, 6), -- 1
        ( 7, 0, 2, 1),
        ( 8, 0, 3, 2),
        ( 9, 0, 4, 3),
        (10, 0, 5, 4),
        (11, 0, 6, 5),
        (12, 0, 1, 6), -- 2
        (13, 0, 8, 7),
        (14, 0, 9, 8),
        (15, 0,10, 9),
        (16, 0,11,10),
        (17, 0,12,11),
        (18, 0, 7,12), -- 3
        (19, 0,14,13),
        (20, 0,15,14),
        (21, 0,16,15),
        (22, 0,17,16),
        (23, 0,18,17),
        (24, 0,13,18), -- 4
        (19, 0,23,22),
        (22, 0,20,19)  -- 5

      );

  begin
    meteor01:= new Object_3D( Max_points=> 24,
                              Max_faces => meteor01_smmts'Length(1) );

    meteor01.baseobj:=
      ( 1=> ( -20,  10,  55),
        2=> ( -10,  30,  45),
        3=> (  10,  40,  40),
        4=> (  20, -10,  65),
        5=> (   0, -60,  65),
        6=> ( -20, -40,  61),
        7=> ( -40,  20,  30),
        8=> ( -20,  60,  10),
        9=> (  20,  80,   0),
       10=> (  40, -20,  50),
       11=> (   0,-120,  50),
       12=> ( -40, -80,  40),
       13=> ( -40,  20, -40),
       14=> ( -20,  60, -40),
       15=> (  20,  80, -40),
       16=> (  40, -20, -40),
       17=> (   0,-120,  40),
       18=> ( -40, -80,  34),
       19=> ( -20,  10, -60),
       20=> ( -10,  30, -60),
       21=> (  10,  40, -60),
       22=> (  20, -10, -60),
       23=> (   0, -60, -39),
       24=> ( -20, -40, -42) );

      cmin:=   2; cmax:=  18;

      for i in meteor01_smmts'Range(1) loop
        meteor01.faces(i):=
         ( (meteor01_smmts(i,1), meteor01_smmts(i,4),
            meteor01_smmts(i,3), meteor01_smmts(i,2) ),
          textured,
          bright, null, meteor_texture, 1,1,
          int_min,int_max, cmin, cmax );
      end loop;

      xprop:= 2.1 * fl_screen_virtual_size / 600.0;
      yprop:= 2.8 * fl_screen_virtual_size / 600.0;
      zprop:= 2.3 * fl_screen_virtual_size / 400.0;

      for i in 1..meteor01.num_of_points loop
        meteor01.baseobj(i).X:=
                 Integer(Float(meteor01.baseobj(i).X) * xprop);
        meteor01.baseobj(i).Y:=
                 Integer(Float(meteor01.baseobj(i).Y) * yprop);
        meteor01.baseobj(i).Z:=
                 Integer(Float(meteor01.baseobj(i).Z) * zprop);
      end loop;

      meteor01.center:= (0.0,0.0,fl_screen_virtual_size);
      meteor01.id:= "Meteore 01      ";
      Init_object(meteor01.all);

    end Init;

end Meteor;
