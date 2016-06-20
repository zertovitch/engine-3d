with SVGA;                              use SVGA;
with SVGA.Effects;                      use SVGA.Effects;

package body Icosaedron is

  procedure Init is

    use Engine_3D;

    int_min: constant intensity:= (90 * intensity'First) / 100;
    int_max: constant intensity:= (20 * intensity'Last)  / 100;
    cmin, cmax: Color_type:= 0;

    nb_points: constant:= 12;
    nb_faces:  constant:= 20;

    obj_points: array( 1..nb_points, 1..3) of Float :=
       (        ( 0.0,        0.0,       2.0),
                ( 1.78885,    0.0,       0.894427),
                ( 0.552786,   1.7013,    0.894427),
                (-1.44721,    1.05146,   0.894427),
                (-1.44721,   -1.05146,   0.894427),
                ( 0.552786,  -1.7013,    0.894427),
                ( 1.44721,    1.05146,  -0.894427),
                (-0.552786,   1.7013,   -0.894427),
                (-1.78885,    0.0,      -0.894427),
                (-0.552786,  -1.7013,   -0.894427),
                ( 1.44721,   -1.05146,  -0.894427),
                ( 0.0,        0.0,      -2.0));

    obj_faces: constant array( 1..nb_faces, 1..3) of Positive :=
       (  (  3,  1,  2 ),
          (  4,  1,  3 ),
          (  5,  1,  4 ),
          (  6,  1,  5 ),
          (  2,  1,  6 ),
          (  3,  2,  7 ),
          (  8,  3,  7 ),
          (  4,  3,  8 ),
          (  9,  4,  8 ),
          (  5,  4,  9 ),
          ( 10,  5,  9 ),
          (  6,  5,  10 ),
          ( 11,  6,  10 ),
          (  7,  2,  11 ),
          (  2,  6,  11 ),
          (  7, 12,   8 ),
          (  8, 12,   9 ),
          (  9, 12,  10 ),
          ( 10, 12,  11 ),
          ( 11, 12,   7 ) );

    f: constant:= fl_screen_virtual_size * 0.15;

    begin
      icosaedron_obj:=
        new Object_3D( Max_points=> nb_points, Max_faces=> nb_faces );

      for i in reverse 1..nb_points loop
        icosaedron_obj.baseobj(i).X:= Integer( obj_points(i,1) * f );
        icosaedron_obj.baseobj(i).Y:= Integer( obj_points(i,2) * f );
        icosaedron_obj.baseobj(i).Z:= Integer( obj_points(i,3) * f );
      end loop;

      for i in reverse 1..nb_faces loop
        case i mod 10 is
          when 0 => cmin:=   0; cmax:=  31;
          when 1 => cmin:=  32; cmax:=  63;
          when 2 => cmin:=  64; cmax:=  95;
          when 3 => cmin:=  96; cmax:= 127;
          when 4 => cmin:= 128; cmax:= 143;
          when 5 => cmin:= 144; cmax:= 159;
          when 6 => cmin:= 160; cmax:= 191;
          when 7 => cmin:= 192; cmax:= 207;
          when 8 => cmin:= 208; cmax:= 223;
          when 9 => cmin:= 224; cmax:= 239;
          when others => null;
        end case;

        icosaedron_obj.faces(i):=
         ( (obj_faces(i,1), obj_faces(i,2), obj_faces(i,3), 0) ,
           colours, neutral, null, null, 1,1, int_min,int_max, cmin,cmax );
      end loop;

      icosaedron_obj.center:= (0.0,0.0,fl_screen_virtual_size);
      icosaedron_obj.id:= "icosaedron_obj  ";
      Init_object(icosaedron_obj.all);

    end Init;

end Icosaedron;
