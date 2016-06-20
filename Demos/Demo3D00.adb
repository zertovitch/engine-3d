------------------------------------------------------------------------------
--  File:            Demo3D00.adb
--  Description:     Demo/test for Engine_3D package
--  Date / Version:  16-Jul-2001 ; 9-Apr-2001 ;...; 29.VIII.1999
--  Author:          G. de Montmollin
--
--  Based on Peroxide / Telemachos tutorial #4 & Pascal sample
------------------------------------------------------------------------------

with SVGA;                              use SVGA;
with SVGA.IO;                           use SVGA.IO;
with SVGA.Effects;                      use SVGA.Effects;
with SVGA.Effects.IO;                   use SVGA.Effects.IO;
with Game_Colors;                       use Game_Colors;

with Engine_3D;                         use Engine_3D;
with Engine_3D.Math;                    use Engine_3D.Math;
with Engine_3D.Merging;                 use Engine_3D.Merging;
with Engine_3D.Morphing;                use Engine_3D.Morphing;
with Engine_3D.Options;

with Icosaedron, Vehic001, X29, Shuttle3;
with Scanline_profiler;

with Multi_keys;                        use Multi_keys;
with PC_Mouse;                          use PC_Mouse;
with Fine_Timer;
with Time_log, Menu_Graphics_mode;

with Zip;                               use Zip;
with Unzip.Streams;                     use Unzip.Streams;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_functions; use Ada.Numerics.Elementary_functions;


procedure Demo_3D_00 is

  subtype Real is Engine_3D.Real; -- (and not SVGA.Effects.Real)
  package RIO is new Float_IO(Real);       use RIO;

  ----------------------------
  -- Some constant switches --
  ----------------------------

  synchronized_rotations: constant Boolean:= True;
  -- true: uses fine timing package

  -- for debugging via log-file
  make_log: constant Boolean:= False;
  log_name: constant String:= "3d.log";
  log_file: file_type;

  -- FPS result file
  fps_file_name: constant String:= "demo3d00.fps";
  fps_file: file_type;

  show_title: constant Boolean:= True;

  ---------
  side_left_tex, side_right_tex,
  wing_left_tex, wing_right_tex: p_p_Texture_map; -- textures for shuttle

  type Texture_list is array(Natural range <>) of p_p_Texture_map;

  empty_tex: Texture_list(1..0);
  Doom_tex, Duke3D_tex, checker_tex: Texture_list(1..6);
  Sky_tex, Psc_tex, Torus_tex, Grid_tex: Texture_list(1..1);

  Doom_pal, Sky_pal, Psc_pal, multi_pal: Color_Palette;

  zip_data: constant String:= "D3D_Data.zip";
  zif: Zip.zip_info; -- zip directory structure for fast access

  procedure Load_zipped_BMP_texture(name: String; Texture: out p_p_Texture_map) is
    f: Zipped_File_Type;
  begin
    Open(f,zif,name);
    Texture:= new p_Texture_map;
    Load_BMP_texture( Stream(f), Texture.all );
    Close(f);
  end Load_zipped_BMP_texture;

  procedure Load_zipped_BMP_palette(name: String; Palette:  out Color_Palette) is
    f: Zipped_File_Type;
    width: X_Loc; height: Y_Loc;
  begin
    Open(f,zif,name);
    Read_BMP_Header( Stream(f), width, height);
    Load_BMP_palette( Stream(f), Palette );
    Close(f);
  end Load_zipped_BMP_palette;

  pala : constant:= 15;
  palz : constant:= 254;
  palaz: constant:= palz-pala;

  procedure Prepare_checker_textures is
    c: Color_type;
  begin
    for i in checker_tex'Range loop
      declare
        extra: constant Integer:= 6-i;
        shift: constant Integer:= 2**extra;
        bits : constant Integer:= i+1+extra; -- 1 -> 2x2 checker table
        lmax : constant Integer:= 2**bits - 1;
      begin
        checker_tex(i)    := new p_Texture_map;
        checker_tex(i).all:= new Texture_map( bits, bits );
        for ix in 0..lmax loop
         for iy in 0..lmax loop
          if ((ix/shift) mod 2) = ((iy/shift) mod 2) then
           c:= 14;
          else
           c:= Color_type(pala + 20 + (i-1)*40);
          end if;
          Set_Pixel( checker_tex(i).all.all, ix, iy, c);
         end loop;
        end loop;
      end;
    end loop;
  end Prepare_checker_textures;

  ----------
  -- Cube --
  ----------

  cube: Object_3D( Max_points=> 8, Max_faces=> 6 );
  z_cube_max: Integer;

  procedure Init_cube is
    t: constant Integer:= screen_virtual_size / 5;
    c: constant Color_Type:= standard_black; -- dummy colour
    int_min: constant intensity:= intensity'First;
    int_max: constant intensity:= intensity'Last;
    degr: degrade_description(1..6);
  begin
     z_cube_max:= 1 + Integer( Float(t) * 1.732 );

     cube.baseobj:=
      ( (-t,-t,-t), ( t,-t,-t), (-t, t,-t), ( t, t,-t),
        (-t,-t, t), ( t,-t, t), (-t, t, t), ( t, t, t));

     cube.faces:=
      ( ((1,3,4,2), textured,  neutral, null, null, 1,1, int_min,int_max, c,c),
        ((2,4,8,6), colours , neutral, null, null, 1,1, int_min,int_max, c,c),
        ((5,6,8,7), textured,  neutral, null, null, 1,1, int_min,int_max, c,c),
        ((1,5,7,3), colours , neutral, null, null, 1,1, int_min,int_max, c,c),
        ((1,2,6,5), textured,  neutral, null, null, 1,1, int_min,int_max, c,c),
        ((3,7,8,4), colours , neutral, null, null, 1,1, int_min,int_max, c,c) );

     cube.center:= (0.0,0.0,fl_screen_virtual_size);

     -- We will update it when displaying textures and colours together
     for f in 1..6 loop
       cube.faces(f).color_min:=  Color_type(pala + (f-1) * 40);
       cube.faces(f).color_max:=  cube.faces(f).color_min + 31;
       degr(f):= ( cube.faces(f).color_min, 31 );
     end loop;

     cube.id:= "Simple cube     ";
     Init_object(cube);
     Calculate_main_intensity_map( degr );  -- used by checker texture

  end Init_cube;

  -----------
  -- Torus --
  -----------

  nga: constant:= 13;
  npa: constant:= 9;

  torus: Object_3D( Max_points=> nga*npa, Max_faces=> nga*npa );

  procedure Init_torus is
    gr: constant Float:= fl_screen_virtual_size * 0.3;  -- big radius
    pr: constant Float:= gr * 0.2;  -- small radius
    ga, pa, rr: Float;
    ind, p1,p2,p3,p4: Integer;
    int_min: constant intensity:= (22 * intensity'First) / 100;
    int_max: constant intensity:= (22 * intensity'Last)  / 100;
  begin
     for cp in 0..npa-1 loop
       pa:= 2.0*pi * Float(cp) / Float(npa);
       for cg in 0..nga-1 loop
         ga:= 2.0*pi * Float(cg) / Float(nga);
         ind:= 1 + cg + nga * cp;
         rr:= gr + pr * cos(pa);

         torus.baseobj( ind ):=
           (      Integer(rr * cos(ga)),
                  Integer(rr * sin(ga)),
                  Integer(pr * sin(pa)) );

         p1:= cg             + nga * cp;
         p2:= (cg+1) mod nga + nga * cp;
         p3:= (cg+1) mod nga + nga * ((cp+1) mod npa);
         p4:= cg             + nga * ((cp+1) mod npa);

         torus.faces( ind ):=
           ( (1+p1,1+p2,1+p3,1+p4), textured, neutral,
             null, null, 1,1, int_min,int_max, 2,2);
       end loop;
     end loop;

     torus.center:= (0.0,0.0,fl_screen_virtual_size);
     torus.id:= "Torus           ";

     Init_object(torus);

  end Init_torus;

  -------------------------
  -- Multiple icosadrons --
  -------------------------

  multico: p_Object_3D;

  procedure Init_multico is
    use Icosaedron;
    d: constant:= fl_screen_virtual_size * 0.4; -- a certain distance
    ico_centered: Object_3D:= Icosaedron_obj.all;
  begin
    ico_centered.center:= (0.0,0.0,0.0);
    declare
      ico_textured: Object_3D:= ico_centered;
    begin
      for i in 1..ico_textured.Num_of_faces loop
        ico_textured.faces(i).draw_mode:= textured;
        ico_textured.faces(i).rep_U:= 10;
        ico_textured.faces(i).rep_V:= 10;
      end loop;

     multico:= 0.5 * ico_centered +      -- original icosaedron
       1.0 * ico_textured +              -- plus a textured one
       (0.1 * ico_centered + Vector3'(d,0.0,0.0)) + -- plus "moons"
       (0.2 * ico_centered + Vector3'(-0.5*d, 0.866*d,0.0)) +
       (0.3 * ico_centered + Vector3'(-0.5*d,-0.866*d,0.0));

      multico.center:= (0.0,0.0,fl_screen_virtual_size);
    end;
  end Init_multico;

  ---------------------------
  -- Waves, with triangles --
  ---------------------------
  package Waves is
    waves_obj: Engine_3D.p_Object_3D;
    procedure Init;
  end Waves;

  package body Waves is
    procedure Init is
      nf: constant:= 6;
      nsub: constant array(1..6) of Positive:= (1,3,1,3,2,2);

      np,ns: Natural;

      ta: constant Float:= Float(screen_virtual_size) * 0.2;
      ind0, p1,p2,p3,p4: Integer;
      x,y,z,vx,vy,vz: Float;
      int_min: constant intensity:= (22 * intensity'First) / 100;
      int_max: constant intensity:= (22 * intensity'Last)  / 100;
    begin
      ns:= 0;
      np:= 0;
      for cf in 1..nf loop
        np:= np + 2*nsub(cf)**2;
        ns:= ns + (nsub(cf)+1)**2;
      end loop;

      waves_obj:=
        new Object_3D( Max_points=> ns, Max_faces=> np );

      ns:= 0;
      np:= 0;
      for cf in 1..nf loop -- faces

        for cy in 0..nsub(cf) loop
          y:= (Float(cy) / Float(nsub(cf))) * 2.0 - 1.0;
          for cx in 0..nsub(cf) loop
            x:= (Float(cx) / Float(nsub(cf))) * 2.0 - 1.0;
            z:= -0.5 * ( x + 1.0 ) * ( x - 1.0 ) * ( cos( 0.5*pi*y ) );

            case cf is
              when 1 => vx:=  x;        vy:= -1.0 - z; vz:=  y;
              when 2 => vx:=  1.0 + z;  vy:=  x;       vz:=  y;
              when 3 => vx:= -x;        vy:=  1.0 + z; vz:=  y;
              when 4 => vx:= -1.0 - z;  vy:= -x;       vz:=  y;
              when 5 => vx:= -x;        vy:=  y;       vz:= -1.0 - z;
              when 6 => vx:=  x;        vy:=  y;       vz:=  1.0 + z;
              when others => null;
            end case;

            ind0:= cx + (nsub(cf)+1) * cy + ns;

            waves_obj.baseobj( ind0 + 1 ):=
                 ( Integer(ta * vx),
                   Integer(ta * vy),
                   Integer(ta * vz) );

            if cx<nsub(cf) and then cy<nsub(cf) then
              p1:= ind0 + 1;
              p2:= p1 + 1;
              p3:= p2 + (nsub(cf)+1);
              p4:= p1 + (nsub(cf)+1);

              ind0:= 2*(cx + nsub(cf) * cy) + np;
              waves_obj.faces( 1 + ind0):=
                ( (p1,p2,p3, 0), textured, neutral,
                  null, null, 1,1, int_min,int_max, 2,2);
              waves_obj.faces( 2 + ind0):=
                ( (p1, 0,p3,p4), textured, neutral,
                  null, null, 1,1, int_min,int_max, 2,2);

            end if;

          end loop;
        end loop;
        np:= np + 2*nsub(cf)**2;
        ns:= ns + (nsub(cf)+1)**2;
      end loop;

      waves_obj.center:= fl_screen_virtual_size * (0.0,0.0,1.0);
      waves_obj.id:= "Waves--triangles";

      Init_object(waves_obj.all);

    end Init;
  end Waves;

  procedure Prepare_shuttle is
    use Shuttle3;
  begin
    Shuttle3.Init;
    declare
      f: Face_array renames shuttle3_obj.Faces;
    begin
      f(wing_right_a).texture:=  wing_right_tex;
      f(wing_right_a).draw_mode:= textured;
      f(wing_right_b).texture:=  wing_right_tex;
      f(wing_right_b).draw_mode:= textured;
      f(wing_left_a).texture:=  wing_left_tex;
      f(wing_left_a).draw_mode:= textured;
      f(wing_left_b).texture:=  wing_left_tex;
      f(wing_left_b).draw_mode:= textured;
      f(side_left).texture:=  side_left_tex;
      f(side_left).draw_mode:= textured;
      f(side_right).texture:=  side_right_tex;
      f(side_right).draw_mode:= textured;
      Init_object(shuttle3_obj.all);
    end;
  end Prepare_shuttle;

  procedure Demo_3D_00_in_graphics is

    background_colour: color_type;

    pseudo_parallel_src: array(1..max_parallel_lights) of Point;

    procedure Reset_lightings is
    begin
      parallel_lights:= 0;
    end;

    procedure Modify_latest_light(x,y: Integer) is
    begin
      if parallel_lights >0 then
        pseudo_parallel_src(parallel_lights):= (x,y,screen_virtual_size);
        Find_light_direction( pseudo_parallel_src(parallel_lights),
                              (0,0,2*screen_virtual_size),
                              parallel_light_vect(parallel_lights) );
        parallel_light_force(parallel_lights):= 1.0;
      end if;
      if make_log then
       Open(log_file, append_file, log_name);
       Put_Line(log_file, "//lights:");
       for i in 1..parallel_lights loop
         Put(log_file, i,3);
         Put(log_file,"  x:"); Put(log_file,parallel_light_vect(i)(1),3,5,0);
         Put(log_file,"  y:"); Put(log_file,parallel_light_vect(i)(2),3,5,0);
         Put(log_file,"  z:"); Put(log_file,parallel_light_vect(i)(3),3,5,0);
         New_Line(log_file);
       end loop;
       New_Line(log_file);
       Close(log_file);
      end if;
    end Modify_latest_light;

    procedure Add_light(x,y: Integer) is
    begin
      if parallel_lights < max_parallel_lights then
        parallel_lights:= parallel_lights + 1;
        Modify_latest_light(x,y);
        parallel_light_force(parallel_lights):= 1.0;
      end if;
    end Add_light;

    -- Create palette for the cube

    procedure Multicolor is
      c, s, ccol: Color_Type;
      subpal: constant Color_Type:= (palaz+1) / 6;
      s01: Float;
    begin
      Multi_pal(255):= (50,55,45);
      Multi_pal(14) := (63,63,63);
      for i in Color_type'(pala) .. Color_type'(palz) loop
        c:= i - pala;
        s01:= Float(c mod subpal) / Float(Intensity'Last-1);
        if s01 > 1.0 then s01:= 1.0; end if;
        s:=      Color_type(63.0 * s01);
        ccol:=   c  /  subpal;
        case ccol is
          when 0=> Multi_pal(i):= (s,s,s); -- grey scale
          when 1=> Multi_pal(i):= (s,0,0); -- red scale
          when 2=> Multi_pal(i):= (0,s,0); -- green scale
          when 3=> Multi_pal(i):= (0,0,s); -- blue scale
          when 4=> Multi_pal(i):= (0,s,s); -- cyan scale
          when 5=> Multi_pal(i):= (s,0,s); -- magenta scale
          when others => null;
        end case;
      end loop;
    end Multicolor;

    procedure PurplePal is
    begin
      for i in Color_type'(0) .. 63 loop
        Set_Color(i + pala, (i,0,i));   -- 63 shades from black to purple
      end loop;
    end;

    procedure FakePhongPal is
    begin
      for i in Color_type'(0) .. 63 loop
        Set_Color(i + pala, (i, 10+Color_type(Float(i)/1.4),
                                20+Color_type(Float(i)/1.6) ) );
      end loop;
    end;

    -- Fine timing
    freq: constant:= 600.0; -- Hz

    -- Frames-per-second recording

    T1,T2: time;     -- "Ada" clock for non-synchronized mode
    E1,E2: Float;    -- Fine_time times in synchronized mode
    frames: Natural;
    rec: Natural:= 0;

    type FPS_info is record
      rate: Float;
      name: String(1..100);
      nlen: Natural;
    end record;

    FPS: array(1..100) of FPS_info;

    procedure Reset_FPS is
    begin
      frames:= 0;
      if synchronized_rotations then
        E1:= Float(Fine_Timer.Counter) / freq;
      else
        T1:= Clock;
      end if;
    end Reset_FPS;

    procedure Store_FPS(name: String) is
      secs: Float;
    begin
      if synchronized_rotations then
        E2:= Float(Fine_Timer.Counter) / freq;
        secs:= E2-E1;
      else
        T2:= Clock;
        secs:= Float(T2-T1);
      end if;
      rec:= rec + 1;
      if secs= 0.0 then
        FPS(rec).rate:= 0.0;
      else
        FPS(rec).rate:= Float(frames) / secs;
      end if;
      FPS(rec).name(1..name'Length):= name;
      FPS(rec).nlen:= name'Length;
    end Store_FPS;

    -- This is an off-screen frame buffer
    type Frame_Access is access Screen_Buffer;
    Buffer : Frame_Access;    -- The buffer

    procedure Show_Lights is
      PA,PB: ScrPoint; ok: Boolean;
    begin
      for i in 1 .. parallel_lights loop
        Project( pseudo_parallel_src(i), PA, ok);
        Project( (0,0,2*screen_virtual_size), PB, ok);
        Line( buffer.all, X_loc(PA.x),Y_loc(PA.y), X_loc(PB.x),Y_loc(PB.y) );
      end loop;
    end Show_Lights;

    Xrot,Yrot,Zrot : Integer:= 0; -- object rotation

    previously_left_button_pressed: Boolean:= False;

    procedure Mouse_and_lights is
      btns: Mouse_button_bunch;
      mx,my: Integer;
    begin
      if MouseInstalled then     -- Mouse, for lightings
        Mouse( mx, my, btns );

        if btns( left ) then
          mx:= mx - X_Size/2;
          my:= my - Y_size/2;
          mx:= (screen_virtual_size * mx) / X_Size;
          my:= (screen_virtual_size * my) / Y_Size;
          if previously_left_button_pressed then
            Modify_latest_light(mx,my);
          else
            Add_light(mx,my);
          end if;
          Show_lights;
        elsif btns( right ) then
          Reset_lightings;
        end if;

        previously_left_button_pressed:= btns( left );
      end if;
    end Mouse_and_lights;

    skip_to_next: Boolean;
    snaps: Natural:= 0;
    abort_demo: exception;

    speed: Float:= 40.0;
    speed_factor: constant Float:= 2.0 ** (1.0/1.666); -- 1.666 sec. -> 2x speed
    totrot: Float:= 0.0;
    last_time: Float:= 0.0;

    procedure Common_ops(o: in out Object_3D) is
      elaps, fine_time: Float;
      ext: String(1..4);
      rota: constant:= 0.5*pi;
      pas: Real;

      function take_key( scancode: key_value ) return Boolean is
        begin
          if synchronized_rotations then
            return keyboard( scancode ); -- we can proportion
          else
            return Strike_1( scancode ); -- we can't -> separate strikes
          end if;
        end;

      begin
        Mouse_and_lights;
        Put_Buffer(Buffer.all);     --  Copy the buffer to the screen

        if Strike_1( 45 ) then -- 'x' on US and other keyboards
          snaps:= snaps + 1;
          Put(To=> ext, Item=> 1000 + snaps);
          Save_BMP("snap_" & ext(2..4) & ".bmp", Buffer.all, Get_Palette);
        end if;

        Clear_Screen( Buffer.all, background_colour );  --  Clear the buffer

        o.auto_rotation:=
           XYZ_rotation( Engine_3D.Real(Xrot)*pi/1800.0,
                         Engine_3D.Real(Yrot)*pi/1800.0,
                         Engine_3D.Real(Zrot)*pi/1800.0 );

        if synchronized_rotations then
          -- Real-time synchronized speed
          fine_time:= Float(Fine_Timer.Counter) / freq;
          elaps:= fine_time - last_time;

          totrot:= totrot + elaps * speed;
          while totrot > 3600.0 loop totrot:= totrot - 3600.0; end loop;
          Xrot  := Integer(totrot *  9.0) mod 3600;
          Yrot  := Integer(totrot * 21.0) mod 3600;
          Zrot  := Integer(totrot *  7.0) mod 3600;

          last_time:= fine_time;

        else  -- unsynchronized mode

          Xrot  := (Xrot + 9)  mod 3600;
          Yrot  := (Yrot + 21) mod 3600;
          Zrot  := (Zrot + 7)  mod 3600;

          elaps:= 1.0;
        end if;

        if keyboard( key_esc ) then raise abort_demo; end if;

        skip_to_next:= False;
        if Strike_1( key_space ) then
          skip_to_next:= True;
        end if;

        if take_key( key_gray_plus ) then  -- * speed_factor after 1 second
          speed:= Exp( Log(speed) + Log(speed_factor) * elaps );
        end if;
        if take_key( key_gray_minus ) then -- / speed_factor after 1 second
          speed:= Exp( Log(speed) - Log(speed_factor) * elaps );
        end if;
        if take_key( key_left ) then
          World_rotation:= XYZ_rotation( 0.0, -rota*Real(elaps),0.0 ) *
                           World_rotation;
        end if;
        if take_key( key_right ) then
          World_rotation:= XYZ_rotation( 0.0, +rota*Real(elaps),0.0 ) *
                           World_rotation;
        end if;
        if take_key( 31 ) then -- swing -
          World_rotation:= XYZ_rotation( 0.0,0.0,-rota*Real(elaps) ) *
                           World_rotation;
        end if;
        if take_key( 32 ) then -- swing +
          World_rotation:= XYZ_rotation( 0.0,0.0,+rota*Real(elaps) ) *
                           World_rotation;
        end if;
        if take_key( 30 ) then -- turn up
          World_rotation:= XYZ_rotation( -rota*Real(elaps),0.0,0.0 ) *
                           World_rotation;
        end if;
        if take_key( 44 ) then -- turn down
          World_rotation:= XYZ_rotation( +rota*Real(elaps),0.0,0.0 ) *
                           World_rotation;
        end if;
        pas:= fl_screen_virtual_size * Real(elaps);
        if take_key( key_up ) then
          Eye:= Eye + pas * (Transpose(World_rotation) * (0.0,0.0,1.0));
        end if;
        if take_key( key_down ) then
          Eye:= Eye - pas * (Transpose(World_rotation) * (0.0,0.0,1.0));
        end if;

        Orthonormalize(World_rotation);
        Rotate_lights;
        o.sorting_refreshed:= False;

     end Common_ops;

    procedure Texture_demo(
        title      : String;
        o          : in out Object_3D;
        tl         : Texture_list;
        P          : Color_Palette;
        sh         : shading_mode;
        surf_select: surface_select;
        mm         : texture_mapping_mode
     )
       is

       texture_index: Positive;

       function Title_and_details return String is
       begin
         return title &
                Integer'Image(o.num_of_points) & " points," &
                Integer'Image(o.num_of_faces)  & " polys" &
                ' ' & surface_select'Image( surf_select ) &
                " / " & texture_mapping_mode'Image(mm) &
                " / " & shading_mode'Image(sh);
       end Title_and_details;

    begin
       if tl'Length /=0 then -- non-empty texture list:
         -- We set one texture to one face:
         texture_index:= tl'First;
         for f in 1..o.Num_of_faces loop
           o.Faces(f).texture:= tl(texture_index);
           texture_index:= texture_index + 1;
           if texture_index > tl'Last then texture_index:= tl'First; end if;
         end loop;
       end if;

       Init_object(o);
         -- 15-Jul-2001 re-init object:texture status change alters invariants

       Set_Palette(P);
       Reset_FPS;
       loop
         Common_ops(o);
         case mm is
           when npersp_y_affine_x =>    Yrot:= 0;   Zrot:= 0;
           when affine_y_npersp_x =>    Xrot:= 0;   Zrot:= 0;
           when others => null;
         end case;

         Draw(Buffer.all, o, surf_select, mm, sh);
         if show_title then
           Write_String(Buffer.all, 0,0, Title_and_details, 255);
         end if;

         exit when skip_to_next;
         frames:= frames + 1;
       end loop;
       Store_FPS(Title_and_details);
    end Texture_demo;

  begin --  Demo_3D_00_in_graphics

   begin -- block wrapping abort_demo

    if MouseInstalled then
      G_Initialize( 0, 0, X_Max, Y_Max );
      HideMouse;
    end if;

    Buffer:= new Screen_Buffer(X_Size,Y_Size);
    background_colour:= standard_black;

    Init_Engine( X_res=> X_Size,
                 Y_res=> Y_Size,
                 X_clip_left=> 0, X_clip_right=>  X_Max,
                 Y_clip_top=>  0, Y_clip_bottom=> Y_Max,
                 Z_clip_min=>  fl_screen_virtual_size * 0.1,
                 X_offset=> X_Size/2,
                 Y_offset=> Y_Size/2,
                 Focal=> fl_screen_virtual_size );

    Eye:= (others=> 0.0);   -- eye (again) at centre of the universe...
    World_rotation:= Id33;

    Reset_lightings;
    Add_light(500,250);
    Multi_pal:= Get_Palette; -- to get the standard values 0..15

    -------------- Test all shading methods:
    --  type shading is ( none, Z_shading, Lambert, Gouraud, Phong );

    Multicolor;
    Set_Palette( Multi_pal );

    -- reset cube data (orignal palette + texture repetition)
    Init_cube;

    Multi_keys.Install;

    if synchronized_rotations then
      Fine_Timer.Install;
      Fine_Timer.Set_timer_frequency( freq );
    end if;

    for a_shading in shading_mode loop
      Reset_FPS;
      loop
        Common_ops(cube);
        Draw (Buffer.all, cube, colors_only, affine_y_affine_x,
              a_shading, True,
              screen_virtual_size - z_cube_max * 2,
              screen_virtual_size + z_cube_max * 2);
        if show_title then
          Write_String(Buffer.all, 0,0, "Cube: shading is " &
                        shading_mode'Image(a_shading), 255);
        end if;
        exit when skip_to_next;
        frames:= frames + 1;
      end loop;
      Store_FPS("Cube: shading is " & shading_mode'Image(a_shading) );
    end loop;

    --------------------------
    -- Test texture mapping --
    --------------------------

    Reset_main_intensity_map;

    -- Comparision affine / persp texture with a test picture
    for mm in texture_mapping_mode loop
      Texture_demo( "Checker", cube, checker_tex, multi_pal, Z_only,
                        textures_only, mm );
    end loop;

    -- Display all these nice pictures on our cube...
    Texture_demo( "Sky", cube, sky_tex, sky_pal, Z_only,
                      textures_only, auto );

    ---------------------
    -- Shaded textures --
    ---------------------

    Calculate_main_intensity_map( Doom_intensities );

    -- Suitable intensity percentage ranges:
    for i in 1..cube.num_of_faces loop
      cube.faces(i).intens_min:= (55 * intensity'First) / 100;
      cube.faces(i).intens_max:= (35 * intensity'Last ) / 100;
    end loop;

    Texture_demo( "Doom", cube, Doom_tex, Doom_pal, Lambert,
                  textures_only, auto);

    Texture_demo( "Doom", cube, Doom_tex, Doom_pal, Gouraud,
                  textures_only, auto);

    Calculate_main_intensity_map( Duke3D_intensities );

    -- Suitable intensity percentage ranges:
    for i in 1..cube.num_of_faces loop
      cube.faces(i).intens_min:= (37 * intensity'First) / 100;
      cube.faces(i).intens_max:= (37 * intensity'Last ) / 100;
    end loop;

    Texture_demo( "Duke 3D", cube, Duke3D_tex,
                  Game_palette, Gouraud,
                  textures_only, auto);

    -- Test texture repetition
    for i in 1..cube.num_of_faces loop
      cube.faces(i).rep_U:= i;
      cube.faces(i).rep_V:= cube.num_of_faces+1-i;
    end loop;
    Texture_demo( "Duke 3D", cube, Duke3D_tex,
                  Game_palette, Gouraud,
                  textures_only, auto);

    -- Mixed set textures and colours - on different faces!
    --  first, colour span redef. for the coloured faces:
    cube.faces(2).color_min:=  32;
    cube.faces(2).color_max:=  63;
    cube.faces(4).color_min:=  96;
    cube.faces(4).color_max:= 127;
    cube.faces(6).color_min:= 128;
    cube.faces(6).color_max:= 143;

    Texture_demo( "Duke 3D", cube, Duke3D_tex,
                  Game_palette, Gouraud,
                  auto, auto );

    for FX in Gouraud .. Phong loop
      Texture_demo( "Icosaedron", Icosaedron.Icosaedron_obj.all, Torus_tex,
                    Game_palette, FX, auto, auto );
    end loop;

    Texture_demo( "Multi-icosaedron", multico.all, Grid_tex,
                  Game_palette, Gouraud, auto, auto );

    Texture_demo( "Torus", torus, Torus_tex,
                  Game_palette, Gouraud, auto, auto );

    Texture_demo( "Water", Waves.waves_obj.all, Psc_tex, Psc_pal,
                  Gouraud, auto, auto );

    background_colour:= 133; -- a dark red

    Texture_demo( "Zembla", Vehic001.vehicle_001.all, empty_tex,
                  Game_palette, Gouraud, auto, auto );

    for fx in reflexion_type loop
      for f in Vehic001.vehicle_001.faces'Range loop
        Vehic001.vehicle_001.faces(f).surface_reflexion:= fx;
      end loop;
      Texture_demo( "Zembla - " & reflexion_type'Image(fx),
                    Vehic001.vehicle_001.all, Torus_tex,
                    Game_palette, Gouraud, auto, auto );
    end loop;

    Texture_demo( "X29", X29.X29_obj.all, empty_tex,
                  Game_palette, Gouraud, auto, auto );

    background_colour:= 69; -- a dark blue

    Texture_demo( "Shuttle3", Shuttle3.Shuttle3_obj.all, empty_tex,
                  Game_palette, Gouraud, auto, auto );

   exception
    when abort_demo => null;
   end; -- block wrapping abort_demo

   if synchronized_rotations then
     Fine_Timer.Uninstall;
   end if;
   Multi_keys.Uninstall;

   -- Output of FPS statistics
   for r in 1..rec loop
     Put(fps_file, FPS(r).rate, 4,2,0 );
     Put_Line(fps_file, "   - " & FPS(r).name(1..FPS(r).nlen));
   end loop;

  exception
    when others =>
      if synchronized_rotations then Fine_Timer.Uninstall; end if;
      Multi_keys.Uninstall;
      Text_Mode;
      raise;
  end Demo_3D_00_in_graphics;

  can_draw: Boolean;
  mode: SVGA_mode;

begin
  if make_log then Create(log_file, name=> log_name); Close(log_file); end if;

  Put_Line("      ****************************************************************");
  Put_Line("      *                                                              *");
  Put_Line("      *                 3D OBJECT ENGINE - THE FILLS                 *");
  Put_Line("      *                        by : Telemachos                       *");
  Put_Line("      *                  Extended (Demo_3D_00) by GdM                *");
  Put_Line("      *                                                              *");
  Put_Line("      ****************************************************************");
  New_Line;
  Put_Line("      This demo is based on Peroxide Programming Tips #4");
  Put(     "      Unpacking now textures & palettes...");

  Zip.Load(zif, zip_data);
  -- Load the Doom Palette
  Load_zipped_BMP_palette ("caisse.bmp", Doom_pal);
  -- Load Doom textures
  Load_zipped_BMP_texture( "caisse.bmp",   Doom_tex(1) );
  Load_zipped_BMP_texture( "w111_2.bmp",   Doom_tex(2) );
  Load_zipped_BMP_texture( "wall71_5.bmp", Doom_tex(3) );
  Load_zipped_BMP_texture( "w109_2.bmp",   Doom_tex(4) );
  Load_zipped_BMP_texture( "door9_1.bmp",  Doom_tex(5) );
  Load_zipped_BMP_texture( "w94_1.bmp",    Doom_tex(6) );

  -- Load Duke3D textures
  Load_zipped_BMP_texture( "barres_x.bmp",  Duke3D_tex(1) );
  Load_zipped_BMP_texture( "t004p073.bmp",  Duke3D_tex(2) );
  Load_zipped_BMP_texture( "t004p077.bmp",  Duke3D_tex(3) );
  Load_zipped_BMP_texture( "t004p094.bmp",  Duke3D_tex(4) );
  Load_zipped_BMP_texture( "stones01.bmp",  Duke3D_tex(5) );
  Load_zipped_BMP_texture( "t004p120.bmp",  Duke3D_tex(6) );

  -- Load others
  Load_zipped_BMP_palette ("skyenv.bmp", sky_pal);
  Load_zipped_BMP_texture( "skyenv.bmp", sky_tex(1) );

  Load_zipped_BMP_texture( "t004p113.bmp", Torus_tex(1) );
  Grid_tex(1):= Duke3D_tex(1);

  Load_zipped_BMP_palette ("piscine2.bmp", psc_pal);
  Load_zipped_BMP_texture( "piscine2.bmp", psc_tex(1) );

  Load_zipped_BMP_texture( "sideleft.bmp", side_left_tex );
  Load_zipped_BMP_texture( "siderigh.bmp", side_right_tex );
  Load_zipped_BMP_texture( "wingleft.bmp", wing_left_tex );
  Load_zipped_BMP_texture( "wingrigh.bmp", wing_right_tex );

  Zip.Delete( zif );

  Put_Line(" done.");

  Put_Line("      Checker textures");
  Prepare_checker_textures;

  Put("      Initialize 3D Objects...");
  Init_cube;
  Init_torus;
  Waves.Init;
  Vehic001.Init;
  X29.Init;
  Prepare_shuttle;
  Icosaedron.Init;
  Init_multico;
  Put_Line(" done.");

  Put(     "      Shadings:  ");
  for s in shading_mode loop Put(' ' & shading_mode'Image(s)); end loop;
  New_Line;
  New_Line;

  MouseReset;
  if MouseInstalled then
    Put_Line("[Mouse]     Left button: Add / Move new light. Right button: reset lights");
  else
    Put_Line("Mouse not installed!");
  end if;
  New_Line;
  Put_Line("[Keyboard]  SPACE     : next part");
  Put_Line("            Gray +/-  : object rotation speed control");
  Put_Line("            Left/Right: turn left/right");
  Put_Line("            Up/Down   : forward/backward");
  Put_Line("            " & Key_image(31,True) & '/' & Key_image(32,True) &
           "       : swing angle");  -- US: S/D (country-sensitive key image)
  Put_Line("            " & Key_image(30,True) & '/' & Key_image(44,True) &
           "       : turn up/down"); -- US: A/Z (country-sensitive key image)
  Put_Line("            " & Key_image(45,True) &
           "         : snapshot to snap_???.bmp"); -- US: X
  New_Line;

  begin
    Open(fps_file, append_file, fps_file_name);
  exception
    when name_error => -- file doesn't exist
    Create(fps_file, out_file, fps_file_name);
  end;
  Put_Line(fps_file, "Demo starting - " & Time_log);

  Menu_Graphics_mode(can_draw, mode);
  if can_draw then
    Put_Line(fps_file, "New mode: " & SVGA_mode'Image(mode) );
    Demo_3D_00_in_graphics;
    Text_Mode;
  end if;

  Put_Line(fps_file, "32-bit Linear Frame Buffer: " &
                      Boolean'Image(Linear_frame_buffer));
  New_Line(fps_file);
  Close(fps_file);

  Put_Line("Frames-per-second are stored into file " & fps_file_name);
  Put_Line("32-bit Linear Frame Buffer: " &
            Boolean'Image(Linear_frame_buffer));
  Put_Line("Scanline profiling: " &
            Boolean'Image(Engine_3D.Options.scanline_profiling));
  if Engine_3D.Options.scanline_profiling then
    Scanline_profiler.Finalize; -- write the statistics in a log file
  end if;

  -- Deallocation of big objects (unnecessary in this context)
  Dispose(Vehic001.vehicle_001);
  Dispose(X29.x29_obj);
  Dispose(Shuttle3.Shuttle3_obj);
  Dispose(Waves.waves_obj);

-- exception
--   when others=> if Is_Open(fps_file) then Close(fps_file); end if; raise;
end Demo_3D_00;
