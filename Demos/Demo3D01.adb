------------------------------------------------------------------------------
--  File:            Demo3D01.adb
--  Description:     Demo/test for Engine_3D package - Portals
--  Date / Version:  14-Sep-2003 ; 14-Jan-2002 ;...; 8-Feb-2000
--  Author:          G. de Montmollin
------------------------------------------------------------------------------

with SVGA;                              use SVGA;
with SVGA.IO;                           use SVGA.IO;
with SVGA.Effects;                      use SVGA.Effects;
with SVGA.Effects.IO;                   use SVGA.Effects.IO;
with SVGA.Effects.Bump;                 use SVGA.Effects.Bump;
with Game_Colors;                       use Game_Colors;
with Game_Driving;                      use Game_Driving;

with Engine_3D;                         use Engine_3D;
with Engine_3D.Math;                    use Engine_3D.Math;
with Engine_3D.Physics;                 use Engine_3D.Physics;
with Engine_3D.Morphing;                use Engine_3D.Morphing;
with Engine_3D.Construct;               use Engine_3D.Construct;
with Engine_3D.Options;

with Vehic001, Shuttle3, Meteor;
with Scanline_profiler;

with Multi_keys;                        use Multi_keys;
with PC_Mouse;                          use PC_Mouse;
with Fine_Timer;
with SB_Digi;                           use SB_Digi;
with SB_Driver;
with Menu_Graphics_mode;

with Zip;                               use Zip;
with Unzip.Streams;                     use Unzip.Streams;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_functions; use Ada.Numerics.Elementary_functions;

procedure Demo_3D_01 is

  subtype Real is Engine_3D.Real; -- (and not SVGA.Effects.Real)
  package RIO is new Float_IO(Real);       use RIO;

  zip_data: constant String:= "D3D_Data.zip";
  zif: Zip.zip_info; -- zip directory structure for fast access

  procedure Load_zipped_BMP_texture(name: String; Texture: out p_Texture_map) is
    f: Zipped_File_Type;
  begin
    Open(f,zif,name);
    Load_BMP_texture( Stream(f), Texture );
    Close(f);
  end Load_zipped_BMP_texture;

  procedure Load_zipped_sound(name: String; smp: out p_Sample) is
    f: Zipped_File_Type;
  begin
    Open(f,zif,name);
    smp:= new Sample'( Load_sample( Stream(f), SB_WAV) );
    Close(f);
  end Load_zipped_sound;

  type texture_id is
    ( Beton001, BoisCrac, CadrBrul, CroiCroi,
      FentJaun, Gobelin2, Grilles,  PierBleu,
      Rouille1, Rouille2, Rouille3, Rouille4,
      VerOpakG, Vitre,
      Barres_X,
      T004p073, T004p113, T004p120,
      Garage01, Garage02, Garage03, Garage04, Garage05, Garage06,
      GarageV0, GarageV1, GarageV2, GarageV3,
      GarageVk,
      SideLeft, SideRigh,
      WingLeft, WingRigh,
      Meteor05, Stones01, Grogalet,
      Textur24,
      -- Animated: (not loaded)
      VitreAni, VentilAni, DalleAni
    );

  Vitre_deformee, Dalle_deformee: p_Texture_map;

  subtype loaded_tex_id is texture_id range texture_id'First..Textur24;

  tex: array(texture_id) of p_p_Texture_map;

  -- Sounds
  type sound_id is
    (Teleport, OpenDoor,
     DSDorOpn, DSDorCls, DSStnMov,
     QHealth1, Q1talk);

  orig_sample, corrected_sample: array( sound_id ) of p_Sample:=
    (others=> null);
  SB_inst_OK: Boolean;

  procedure Emit_sound(s: sound_id) is -- !! + volume + optional stereo params
  begin
    if SB_inst_OK then
      Mix_a_sample( corrected_sample( s ) );
    end if;
  end Emit_sound;


  procedure Prepare_shuttle is
    use Shuttle3;
  begin
    Shuttle3.Init;
    declare
      f: Face_array renames shuttle3_obj.Faces;
    begin
      f(wing_right_a).texture:=  tex(WingRigh);
      f(wing_right_a).draw_mode:= textured;
      f(wing_right_b).texture:=  tex(WingRigh);
      f(wing_right_b).draw_mode:= textured;
      f(wing_left_a).texture:=  tex(WingLeft);
      f(wing_left_a).draw_mode:= textured;
      f(wing_left_b).texture:=  tex(WingLeft);
      f(wing_left_b).draw_mode:= textured;
      f(side_left).texture:=  tex(SideLeft);
      f(side_left).draw_mode:= textured;
      f(side_right).texture:=  tex(SideRigh);
      f(side_right).draw_mode:= textured;
      Init_object(shuttle3_obj.all);
    end;
  end Prepare_shuttle;

  ----------
  -- laby --
  ----------

  laby: a_p_Object_3D(1..8);
  clone_shuttle, clone_vehicle: p_Object_3D;
  Room_scale: constant:= 2*screen_virtual_size;

  subtype Wall_Count is Integer range 1..4;
  type Is_a_door     is array(Wall_Count) of Boolean;
  type Door_textures is array(Wall_Count) of Texture_id;
  type Door_faces    is array(Wall_Count) of Positive;
  type Door_connect  is array(Wall_Count) of p_Object_3D;

  subtype garages is Texture_id range Garage01..GarageV0;
  package Rnd_garage is new Ada.Numerics.Discrete_Random(Garages);
  gen: Rnd_Garage.Generator;

  --
  -- Plan du labyrinthe
  --
  --       7 5
  --     8 6 4 2
  --         3 1

  procedure Make_Room( r: in out Object_3D;
                       tex_wall: texture_id;
                       tex_floor: texture_id;
                       tw_rep: Natural;
                       fac: out Door_faces;
                       tid: Door_textures;
                       haut_mur: Positive;
                       cplafond: Color_type) is

   c1: constant Color_Type:= 1;
   c2: constant Color_Type:= 30;
   iad: constant intensity:= intensity'First;
   ibd: constant intensity:= intensity'Last;
   ia: Intensity:= iad;
   ib: constant Intensity:= ibd;
   ia_sol: constant intensity:= intensity'First;
   ib_sol: constant intensity:= intensity'Last;
   seg     : constant:= 4; -- pair!
   p1,p2,p3,p4: Point;
   t: constant:= Room_scale;
   tex_main_wall: texture_id;

   --     1
   --   2 * 4
   --     3

   procedure Rotate_points(w:Wall_Count) is
     begin
       case w is
         when 1 => null;
         when 2 => p1:= (t-p1.y, p1.x,p1.z);
                   p2:= (t-p2.y, p2.x,p2.z);
                   p3:= (t-p3.y, p3.x,p3.z);
                   p4:= (t-p4.y, p4.x,p4.z);
         when 3 => p1:= (t-p1.x,t-p1.y,p1.z);
                   p2:= (t-p2.x,t-p2.y,p2.z);
                   p3:= (t-p3.x,t-p3.y,p3.z);
                   p4:= (t-p4.x,t-p4.y,p4.z);
         when 4 => p1:= ( p1.y,t-p1.x,p1.z);
                   p2:= ( p2.y,t-p2.x,p2.z);
                   p3:= ( p3.y,t-p3.x,p3.z);
                   p4:= ( p4.y,t-p4.x,p4.z);
       end case;
     end Rotate_points;

   begin
     r.Num_of_points:= 0;
     r.Num_of_faces := 0;

     for v in -seg .. seg loop
       for u in -seg .. seg loop
         Create_face(
           ((0,0,0,0), textured, matt, null,
           tex(tex_floor), 1,1, ia_sol,ib_sol, c1,c1),
           ( (u*t,v*t,0), ((u+1)*t,v*t,0),
             ((u+1)*t,(v+1)*t,0), (u*t,(v+1)*t,0) ),
           ( True, True, True, True ),
           r ); -- sol
       end loop;
     end loop;

     for v in -seg .. seg loop
       for u in -seg .. seg loop
        declare
         function z(u,v:Integer) return Integer is
         begin
           return haut_mur*t +  -- coupole 11-Apr-2001
             ( ((2*seg+1)**2 - (2*u-1)**2) *
               ((2*seg+1)**2 - (2*v-1)**2) * t)
            / (2*16*seg**4);
         end z;
        begin
         Create_face(
           ( P        => (0,0,0,0),
             draw_mode => Draw_modes'Val((u+v) mod 2),
             surface_reflexion => bright,
             connecting    => null,
             texture       => tex( T004p073 ),
             rep_U    => 1,
             rep_V    => 1,
             intens_min => ia,
             intens_max => ib,
             color_min  => cplafond,
             color_max  => cplafond +31 ),
           ( (u*t,     v*t,     z(u,  v)),
             (u*t,     (v+1)*t, z(u,  v+1)),
             ((u+1)*t, (v+1)*t, z(u+1,v+1)),
             ((u+1)*t, v*t,     z(u+1,v))
           ),  -- quadri NON-plat!
           ( True, True, True, True ),
           r ); -- plafond
        end;
       end loop;
     end loop;

     for mur in Wall_Count loop
       p1:= (-t,(seg+1)*t,0);
       p2:= (0,(seg+1)*t,0);
       p3:= (0,(seg+2)*t,0);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), textured, neutral, null,
         tex(Beton001), 1,1, ia_sol,ib_sol, c1,c1),
         ( p1,p2,p3,p4 ), ( True, True, True, False ),
         r ); -- seuil g.
       p1:= ( t,(seg+1)*t,0);
       p2:= ( 2*t,(seg+1)*t,0);
       p4:= ( t,(seg+2)*t,0);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), textured, neutral, null,
         tex(Beton001), 1,1, ia_sol,ib_sol, c1,c1),
         ( p1,p2,p3,p4 ), ( True, True, False, True ),
         r ); -- seuil d.
       p1:= ( 0,(seg+1)*t,0);
       p2:= ( t,(seg+1)*t,0);
       p3:= ( t,(seg+2)*t,0);
       p4:= ( 0,(seg+2)*t,0);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), textured, neutral, null,
         tex(Beton001), 1,1, ia_sol,ib_sol, c1,c1),
         ( p1,p2,p3,p4 ), ( True, True, True, True ),
         r ); -- seuil c.

       if tex_wall = t004p113 or
          tex_wall = Rouille4 or
          tex_wall = Rouille3
       then
         ia:= iad/2;
       end if; -- not too dark

       p1:= ( 0,(seg+1)*t,t);
       p2:= ( 0,(seg+2)*t,t);
       p4:= ( 0,(seg+1)*t,2*t);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), textured, neutral, null,
         tex(tex_wall), tw_rep, tw_rep, ia,ib, c1,c2),
          (p1,p2,p3,p4), ( True, True, False, True ),
         r ); -- bout de mur g.

       p2:= ( 0,(seg+2)*t,t);
       p3:= ( 0,(seg+1)*t,t);
       p4:= (-t,(seg+1)*t,t);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), textured, neutral, null,
          tex(tex_wall), tw_rep, tw_rep, ia,ib, c1,c2),
          (p1,p2,p3,p4), ( False, True, True, True ),
         r ); -- bout de mur sous, g.

       p1:= ( t,(seg+2)*t,t);
       p2:= ( t,(seg+1)*t,t);
       p3:= ( t,(seg+1)*t,2*t);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), textured, neutral, null,
          tex(tex_wall), tw_rep, tw_rep, ia,ib, c1,c2),
          (p1,p2,p3,p4), ( True, True, True, False ),
         r ); -- bout de mur d.

       p1:= (  t,(seg+2)*t,t);
       p3:= (2*t,(seg+1)*t,t);
       p4:= (  t,(seg+1)*t,t);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), textured, neutral, null,
          tex(tex_wall), tw_rep, tw_rep, ia,ib, c1,c2),
          (p1,p2,p3,p4), ( True, False, True, True ),
         r ); -- bout de mur sous, d.

       for u in -seg .. seg loop
         for v in 0..haut_mur-1 loop
           p1:= (u*t,(seg+1)*t,v*t);
           p2:= ((u+1)*t,(seg+1)*t,v*t);
           p3:= ((u+1)*t,(seg+1)*t,(v+1)*t);
           p4:= (u*t,(seg+1)*t,(v+1)*t);
           if u=0 and then v=0 then
             p1.y:= p1.y+t;
             p2.y:= p2.y+t;
             p3.y:= p3.y+t;
             p4.y:= p4.y+t;
           elsif u=-1 and then v=0 then
             p2.y:= p2.y+t;
             p3.y:= p3.y+t;
           elsif u=+1 and then v=0 then
             p1.y:= p1.y+t;
             p4.y:= p4.y+t;
           elsif u=0 and then v=1 then
             p1.y:= p1.y+t;
             p2.y:= p2.y+t;
           end if;
           Rotate_points(mur);

           if tex_wall = Rouille4 then
             tex_main_wall:= Rnd_Garage.Random(gen);
             if tex_main_wall = GarageV0 then
               tex_main_wall:= VentilAni;  -- ventilo -> ventilo anime
             end if;
           else
             tex_main_wall:= tex_wall;
           end if;
           Create_face(
             ((0,0,0,0), textured, neutral, null,
              tex( tex_main_wall ), tw_rep, tw_rep, ia,ib, c1,c2),
              (p1,p2,p3,p4), ( True, True, True, True ),
             r ); -- murs
           if u=0 and then v=0 then
             case tid( mur ) is
               when VitreAni => r.faces(r.Num_of_faces).rep_U:= 5;
                                r.faces(r.Num_of_faces).rep_V:= 5;
                                r.faces(r.Num_of_faces).surface_reflexion:=
                                  very_bright;
               when Barres_X => r.faces(r.Num_of_faces).rep_U:= 2;
                                r.faces(r.Num_of_faces).rep_V:= 2;
               when T004p120 => r.faces(r.Num_of_faces).rep_U:= 1;
                                r.faces(r.Num_of_faces).rep_V:= 1;
               when PierBleu => r.faces(r.Num_of_faces).draw_mode:= invisible;
               when others=> null;
             end case;
             fac( mur ):= r.Num_of_faces;
             r.faces(r.Num_of_faces).texture:= tex( tid( mur ) );
           end if;
         end loop;
       end loop;
     end loop;

   end Make_Room;

  -- When all the rooms are made, we have all pointers
  -- for connexion

  procedure Connect_rooms(
      r     : in out Object_3D;
      isidor: Is_a_door;
      fac   : Door_faces;
      por   : Door_connect
    ) is
  begin
    for w in Wall_Count loop
      if isidor(w) then
        r.faces( fac( w ) ).connecting:= por( w );
      end if;
    end loop;
  end Connect_rooms;

  procedure Init_laby is
   use Shuttle3, Vehic001, Meteor;
   Door_faces_of_room  : array( laby'Range ) of Door_faces;
   Door_portals_of_room: array( laby'Range ) of Door_connect;
   isidor              : Is_a_door;
  begin
     Rnd_Garage.Reset(gen);
     for lr in laby'Range loop
       laby(lr):= new Object_3D( Max_points=> 1000, Max_faces=> 1000 );
     end loop;

     Make_Room( r         => laby(3).all,
                tex_wall  => Rouille1,
                tex_floor => DalleAni,
                tw_rep    => 2,
                fac       => Door_faces_of_room(3),
                tid       => (PierBleu, T004p120, BoisCrac, VitreAni),
                haut_mur  => 3,
                cplafond  => start_blue );

     vehicle_001.center:= (Real(-12 * Room_scale),0.0,0.0);
     vehicle_001.auto_rotation:=
        To_Fix(XYZ_rotation( 0.1, -0.02, -0.6 ));

     clone_vehicle:= 5.0 * vehicle_001.all +
                     Vector3'(0.0,0.0,0.5*Real(Room_scale) );

     laby(3).sub_objects:= new Object_3D_list '
       ( objc => clone_vehicle, next => null );

     laby(3).center:= ( Real(-11 * Room_scale), 0.0,0.0);

     Make_Room( r         => laby(2).all,
                tex_wall  => Rouille4,
                tex_floor => Beton001,
                tw_rep    => 1,
                fac       => Door_faces_of_room(2),
                tid       => (T004p120, PierBleu, Barres_X, BoisCrac),
                haut_mur  => 3,
                cplafond  => start_green );

     Shuttle3_obj.center:= (0.0,Real( 12 * Room_scale),0.0);
     Shuttle3_obj.auto_rotation:=
        To_Fix(XYZ_rotation( 0.1, -0.02, -1.0 ));

     clone_shuttle:= 7.5 * Shuttle3_obj.all +
                     Vector3'(0.0,0.0,0.5*Real(Room_scale) );

     laby(2).sub_objects:= new Object_3D_list '
       ( objc => clone_shuttle, next => null );

     laby(2).center:= (0.0, Real( 11 * Room_scale), 0.0);

     Make_Room( r         => laby(1).all,
                tex_wall  => CroiCroi,
                tex_floor => PierBleu,
                tw_rep    => 2,
                fac       => Door_faces_of_room(1),
                tid       => (Barres_X, VitreAni, BoisCrac, BoisCrac),
                haut_mur  => 2,
                cplafond  => start_fires );

     -- 20-Jul-2001: 4th room
     Make_Room( r         => laby(4).all,
                tex_wall  => T004p113,
                tex_floor => PierBleu,
                tw_rep    => 1,
                fac       => Door_faces_of_room(4),
                tid       => (PierBleu, PierBleu, PierBleu, PierBleu),
                haut_mur  => 2,
                cplafond  => start_salmon );

     laby(4).center:= (Real(-11 * Room_scale), Real( 11 * Room_scale), 0.0);

     Meteor.meteor01.center:=
        (Real(-12 * Room_scale),
         Real( 12 * Room_scale),
         1.0);

     laby(4).sub_objects:= new Object_3D_list '
       ( objc => meteor01, next => null );

     -- 11-Jan-2002: rooms 5..8
     Make_Room( r         => laby(5).all,
                tex_wall  => Grogalet,
                tex_floor => PierBleu,
                tw_rep    => 1,
                fac       => Door_faces_of_room(5),
                tid       => (BoisCrac, BoisCrac, PierBleu, BoisCrac),
                haut_mur  => 2,
                cplafond  => start_salmon );

     laby(5).center:= (Real(-11 * Room_scale), 2.0*Real( 11 * Room_scale), 0.0);

     Make_Room( r         => laby(6).all,
                tex_wall  => Rouille3,
                tex_floor => PierBleu,
                tw_rep    => 1,
                fac       => Door_faces_of_room(6),
                tid       => (PierBleu, PierBleu, BoisCrac, PierBleu),
                haut_mur  => 2,
                cplafond  => start_salmon );

     laby(6).center:= (2.0*Real(-11 * Room_scale), Real( 11 * Room_scale), 0.0);

     Make_Room( r         => laby(7).all,
                tex_wall  => Rouille4,
                tex_floor => Stones01,
                tw_rep    => 1,
                fac       => Door_faces_of_room(7),
                tid       => (BoisCrac, BoisCrac, PierBleu, BoisCrac),
                haut_mur  => 6,
                cplafond  => start_salmon );

     laby(7).center:= (2.0*Real(-11 * Room_scale), 2.0*Real( 11 * Room_scale), 0.0);

     Make_Room( r         => laby(8).all,
                tex_wall  => CroiCroi,
                tex_floor => Grogalet,
                tw_rep    => 1,
                fac       => Door_faces_of_room(8),
                tid       => (BoisCrac, BoisCrac, BoisCrac, PierBleu),
                haut_mur  => 4,
                cplafond  => start_salmon );

     laby(8).center:= (3.0*Real(-11 * Room_scale), Real( 11 * Room_scale), 0.0);

     Door_portals_of_room:=
       ( 1 => (laby(2), laby(3), null,    null    ),
         2 => (null,    laby(4), laby(1), null    ),
         3 => (laby(4), null,    null,    laby(1) ),
         4 => (laby(5), laby(6), laby(3), laby(2) ),
         5 => (null,    null,    laby(4), null    ),
         6 => (laby(7), laby(8), null,    laby(4) ),
         7 => (null,    null,    laby(6), null    ),
         8 => (null,    null,    null,    laby(6) )
       );

     for lr in laby'Range loop
       for mur in Wall_count loop
         isidor(mur):= Door_portals_of_room(lr)(mur) /= null;
       end loop;
       Connect_rooms(
         laby(lr).all,
         isidor,
         Door_faces_of_room(lr),
         Door_portals_of_room(lr)
       );
       Put( laby(lr).id, lr );
       laby(lr).id(1..10):= "Labyrinthe";
       Init_object(laby(lr).all);
     end loop;
  end Init_laby;

 procedure Demo_3D_01_in_graphics is

  procedure Reset_lightings is
    begin
      parallel_lights:= 4;
      parallel_light_force(1):= 0.3;
      Find_light_direction( (+50,+10,-20),(0,0,0), parallel_light_vect(1) );
      parallel_light_force(2):= 0.3;
      Find_light_direction( (-50,+10,-20),(0,0,0), parallel_light_vect(2) );
      parallel_light_force(3):= 0.3;
      Find_light_direction( (+10,-50,-10),(0,0,0), parallel_light_vect(3) );
      parallel_light_force(4):= 0.4;
      Find_light_direction( (-10,+50,+10),(0,0,0), parallel_light_vect(4) );

      radial_lights:= 1;
      radial_light_force(1):= 2.8;
    end;

  centrX, centrY: Integer; -- Position souris a l'init.

  -- Fine timing
  freq: constant:= 600.0; -- Hz

  -- This is an off-screen frame buffer
  type Frame_Access is access Screen_Buffer;
  Buffer : Frame_Access;    -- The buffer

  escape: Boolean:= False;

  vitesse, v_later, v_verti: Real:= 0.0;
  dswing, dlater, dverti: Real:= 0.0;
  last_time: Float:= 0.0; -- !! float: mauvais !!

  room_to_draw: Positive:= 1;

  -- Pointers to, alternatively, hieght maps for times k and k+1
  type mapz_pair is array( 0..1 ) of Height_Map_access;
  mapz_vitre, mapz_dalle: mapz_pair;

  cur_map_vitre_ix, cur_map_dalle_ix: Integer:= 0;

  fgen: Generator;

  Xrot,Yrot,Zrot : Integer; -- meteor rotation
  totrot: Float:= 0.0; -- meteor rotation

  meteor_speed: constant:= 30.0;

-- Photo_BMP
  snaps: Natural:= 0;
  procedure Photo_BMP is
    ext: String(1..4);
  begin
    snaps:= snaps + 1;
    Put(To=> ext, Item=> 1000 + snaps);
    Save_BMP("snap_" & ext(2..4) & ".bmp", Buffer.all, Get_Palette);
  end Photo_BMP;

  procedure Process_bump_map(
      mapz       : mapz_pair;
      curr       : in out Integer;
      blob_proba : Float;
      radius     : Positive;
      depth      : Integer;
      boundary   : Boundary_mode;
      propagation: Propagation_mode;
      original   :        Texture_map;
      deformed   : in out Texture_map
    )
  is
  begin
    -- !! synchro !!
    if Random(fgen) > blob_proba then
      Blob(map  => mapz(curr).all,
           mx   => Integer(Random(fgen)*Float(mapz(curr).new_width)),
           my   => Integer(Random(fgen)*Float(mapz(curr).new_height)),
           radius => radius,
           depth  => depth,
           boundary => boundary );
    end if;
    curr:= 1 - curr; -- swap the roles for height maps
    Evolve( mapz(1-curr).all, mapz(curr).all,
            boundary    => boundary,
            propagation => propagation );
    -- !! direction lumiere !!
    Apply( mapz(curr).all,  0, 8, original, deformed );
  end Process_bump_map;


  procedure Common_ops is
    elaps    : Float;
    fine_time: Float; -- !! <- float: mauvais!!
    pas, pas_angle, attenu, attenu_choc: Real;
    c: command_set:= no_command;
    gx, gy: Float;
    step: Vector3;
    landed: p_Object_3D;
    reacted: Float;

    freq_venti: constant:= 27.6543; -- 1/4 tours

    begin
      Put_Buffer(Buffer.all);     --  Copy the buffer to the screen

      Record_commands(c, centrX,centrY, x_size, y_size, gx, gy);

      if c(photo) then -- 'x' on US and other keyboards
        Photo_BMP;
      end if;

      Clear_Screen( Buffer.all, 0 );  --  Clear the buffer

      fine_time:= Float(Fine_Timer.Counter) / freq;
      -- !! pas si fine qd Counter grand !!
      elaps:= fine_time - last_time;

      last_time:= fine_time;

      pas:= 1.15 * fl_screen_virtual_size * Real(elaps);
      if c(run_mode) then
        pas:= pas * 2.0;
      end if;
      pas_angle:= 0.25*pi*Real(elaps);
      attenu:= Real'Max( 0.0, Real'Min(0.96, 1.0 - Real(elaps)*4.0) );
      attenu_choc:= 0.2 * Real'Max(1.0,Real(elaps));

      if c(go_forward)   then vitesse:= vitesse + pas; end if;
      if c(go_backwards) then vitesse:= vitesse - pas; end if;
      if c(go_graduated) then vitesse:= vitesse + pas * Real(gy)*3.0; end if;

      if c(slide_left)   then v_later:= v_later - pas; end if;
      if c(slide_right)  then v_later:= v_later + pas; end if;
      if c(slide_up)     then v_verti:= v_verti - pas; end if;
      if c(slide_down)   then v_verti:= v_verti + pas; end if;
      if c(slide_lateral_graduated)
                         then v_later:= v_later + pas * Real(gx)*5.0; end if;

      if c(turn_left)  then dlater:= dlater - pas_angle; end if;
      if c(turn_right) then dlater:= dlater + pas_angle; end if;
      if c(turn_lateral_graduated)
                       then dlater:= dlater + pas_angle*Real(gx)*2.0; end if;

      if c(turn_up)     then dverti:= dverti - pas_angle; end if;
      if c(turn_down)   then dverti:= dverti + pas_angle; end if;
      if c(swing_plus)  then dswing:= dswing + pas_angle; end if;
      if c(swing_minus) then dswing:= dswing - pas_angle; end if;
      if c(interrupt_game) then escape:= True; end if;

      step:= Transpose(World_rotation)

          -- ^ rotated vision (=inverse of world's rotation)

            * ( v_later,    -- lateral sliding
                v_verti,    -- vertical sliding
                vitesse );  -- forward/backwards

           -- ^ vector in the local referential

      Reaction(
        P0     => Eye,
        radius => 0.9 * fl_screen_virtual_size, -- > focal
        step   => step,
        o      => laby( room_to_draw ).all,
        method => slide,
        reacted => reacted,
        landed_into => landed
      );

      Eye:= Eye + step; -- the "Eye" moves

      World_rotation:= XYZ_rotation( dverti,dlater,dswing ) * World_rotation;

      Orthonormalize(World_rotation);
      vitesse:= vitesse * attenu; -- stabilisation
      v_later:= v_later * attenu;
      v_verti:= v_verti * attenu;
      dswing:=  dswing  * attenu;
      dlater:=  dlater  * attenu;
      dverti:=  dverti  * attenu;

      radial_light_source(1):= Eye; -- lanterne - lampe - headlight - ...
      Rotate_lights;

      -- Useless to sort if convex!!
      if abs(dswing) + abs(dlater) + abs(dverti) > Real'Epsilon then
        for r in laby'Range loop
          laby(r).sorting_refreshed:= False;
        end loop;
      end if;

      -- Animated textures 2-Jan-2002
      -- "Roll" multi-image animation:

      if laby(2).projection_refreshed or   -- !! decision a posteriori
         laby(7).projection_refreshed      -- !! decision a posteriori
      then
        tex(VentilAni).all:=
          tex( Texture_id'Val( Texture_id'Pos(GarageV0) +
                               Integer( freq_venti * fine_time ) mod 4
                             )
          ).all;
      end if;

      -- Process bump mapping:

      if laby(3).projection_refreshed then -- !! decision a posteriori
        Process_bump_map( mapz_vitre, cur_map_vitre_ix, 0.98, 2,23,
                          periodic, wave, tex(Vitre).all.all, Vitre_deformee.all );

        Process_bump_map( mapz_dalle, cur_map_dalle_ix, 0.85, 3,27,
                          periodic, wave, tex(Textur24).all.all, Dalle_deformee.all );
      end if;

      -- Animated objects

      if laby(4).projection_refreshed then -- !! decision a posteriori
        totrot:= totrot + elaps * meteor_speed;
        while totrot > 3600.0 loop totrot:= totrot - 3600.0; end loop;
        Xrot  := Integer(totrot *  9.0) mod 3600;
        Yrot  := Integer(totrot * 21.0) mod 3600;
        Zrot  := Integer(totrot *  7.0) mod 3600;

        Meteor.meteor01.center:=
          (Real(-12 * Room_scale),
           Real( 12 * Room_scale),
           Real(0.75+0.09*Sin(fine_time*0.5))*Real(Room_scale));

        Meteor.meteor01.auto_rotation:=
           XYZ_rotation( Engine_3D.Real(Xrot)*pi/1800.0,
                         Engine_3D.Real(Yrot)*pi/1800.0,
                         Engine_3D.Real(Zrot)*pi/1800.0 );
        Meteor.meteor01.projection_refreshed:= False;
        Meteor.meteor01.sorting_refreshed:= False;
      end if;

      for r in laby'Range loop
        laby(r).projection_refreshed:= False;
      end loop;
      clone_vehicle.projection_refreshed:= False;
      clone_vehicle.sorting_refreshed:= False;
      clone_shuttle.projection_refreshed:= False;
      clone_shuttle.sorting_refreshed:= False;

      if reacted > 0.1 then
        vitesse:= vitesse * attenu_choc;
        v_later:= v_later * attenu_choc;
        v_verti:= v_verti * attenu_choc;
      end if;
      -- Sound (provisorily here)
      if reacted > 0.2 then
        Emit_sound( DSStnMov );
      end if;

    end Common_ops;

  begin
    if MouseInstalled then
      G_Initialize( 0, 0, X_Max, Y_Max );
      MouseXY(centrX,centrY);
      HideMouse;
    end if;

    Emit_sound( QHealth1 );

    Buffer:= new Screen_Buffer(X_Size,Y_Size);

    Init_Engine( X_res=> X_Size,
                 Y_res=> Y_Size,
                 X_clip_left=>   0, X_clip_right => X_Max -   0,
                 Y_clip_top =>   0, Y_clip_bottom=> Y_Max -  32,
                 Z_clip_min =>  fl_screen_virtual_size * 0.1,
                 X_offset=> X_Size/2,
                 Y_offset=> Y_Size/2,
                 Focal=> 1.43*fl_screen_virtual_size ); -- 1.5

    Eye:= (0.0,0.0,fl_screen_virtual_size * 0.98);
    World_rotation:= XYZ_rotation( 0.5*pi,0.0,0.0 );
    Calculate_main_intensity_map( Game_intensities );
    Reset_lightings;
    Set_Palette( Game_palette );
    Multi_keys.Install;
    Fine_Timer.Install;
    Fine_Timer.Set_timer_frequency( freq );

    for i in mapz_vitre'Range loop
      mapz_vitre(i):= new Height_Map(2**tex(Vitre).all.x_bits,
                                     2**tex(Vitre).all.y_bits);
    end loop;
    for i in mapz_vitre'Range loop
      mapz_dalle(i):= new Height_Map(2**tex(Textur24).all.x_bits,
                                     2**tex(Textur24).all.y_bits);
    end loop;
    Reset(fgen);

    loop
      declare
        x,y: Real;
        curr_room: constant Positive:= room_to_draw;
      begin
        -- The most minimalist BSP ever...
        x:= (Eye(1) / Real(Room_scale) + 5.0) / 11.0;
        y:= (Eye(2) / Real(Room_scale) - 6.0) / 11.0;
        if x > 0.0 then
          if y > 0.0 then
            room_to_draw:= 2;
          else
            room_to_draw:= 1;
          end if;
        elsif x <= -2.0 then
          room_to_draw:= 8;
        elsif x <= -1.0 then
          if y > 1.0 then
            room_to_draw:= 7;
          else
            room_to_draw:= 6;
          end if;
        else
          if y > 1.0 then
            room_to_draw:= 5;
          elsif y > 0.0 then
            room_to_draw:= 4;
          else
            room_to_draw:= 3;
          end if;
        end if;
        if room_to_draw /= curr_room then
          Emit_sound( q1talk );
        end if;
      end;

      Common_ops;
      Draw(Buffer.all, laby( room_to_draw ).all, auto, auto, Gouraud);
      exit when escape;
    end loop;

    Fine_Timer.Uninstall;
    Multi_keys.Uninstall;
    if Engine_3D.Options.scanline_profiling then
      Scanline_profiler.Finalize; -- write the statistics in a log file
    end if;

  exception
    when others =>
      Fine_Timer.Uninstall;
      Multi_keys.Uninstall;
      Text_Mode;
      raise;
  end Demo_3D_01_in_graphics;

  can_draw: Boolean;
  mode: SVGA_mode;
  rep_sound: Character;
  want_sound: Boolean;

begin
  Put("Do you wish sounds ?" &
      " (NB: a misconfiguration can cause some noises!) [Y/N] ");
  Get(rep_sound); Skip_Line;
  want_sound:= rep_sound='y' or rep_sound='Y';
  if want_sound then
    SB_Driver.Install( 44100, SB_inst_OK ); -- 11000, 22050, 44100
  else
    SB_inst_OK:= False;
  end if;

  Put("Unpacking data...");

  Zip.Load(zif, zip_data);
  -- [Zip] Load textures
  for tid in loaded_tex_id loop
    tex(tid):= new p_Texture_map;
    Load_zipped_BMP_texture( texture_id'Image(tid) & ".bmp", tex(tid).all );
  end loop;

  -- Deformed textures.
  Vitre_deformee:= new Texture_map(tex(Vitre).all.x_bits,
                                   tex(Vitre).all.y_bits);
  Dalle_deformee:= new Texture_map(tex(Textur24).all.x_bits,
                                   tex(Textur24).all.y_bits);

  -- Create pointers to animated textures
  tex(VitreAni) := new p_Texture_map'(Vitre_deformee); -- won't change
  tex(DalleAni) := new p_Texture_map'(Dalle_deformee); -- won't change
  tex(VentilAni):= new p_Texture_map'(tex(GarageV0).all);
  -- Just a start value; we will switch among images

  -- [Zip] Load Sounds
  if SB_inst_OK then
    for sid in sound_id loop
      Load_zipped_sound( sound_id'Image(sid) & ".wav", orig_sample(sid) );
      if SB_Driver.sample_frequency /= 22727/2 then
        corrected_sample(sid):=
          new Sample'(
            Convert_frequency(
              orig_sample(sid).all,
              22727/2,
              SB_Driver.sample_frequency )
          );
        Free_sample( orig_sample(sid) );
      else
        corrected_sample(sid):= orig_sample(sid);
      end if;
      corrected_sample(sid).left_volume := 32; -- !! prov; valeur a abstraire
      corrected_sample(sid).right_volume:= 32; -- !! prov; valeur a abstraire
    end loop;
  end if;

  Zip.Delete( zif );
  Put_Line(" done.");

  Put("Init things...");
  Vehic001.Init;
  Meteor.Init( tex(Meteor05) );
  Prepare_shuttle;
  Init_laby;
  Put_Line(" done.");
  New_Line;

  MouseReset;
  Put_Line("[Mouse] installed= " & Boolean'Image( MouseInstalled ));
  if MouseInstalled then
    Put_Line("  Left button    : go forward    " &
             "   Move Left/Right: turn -or- slide left/right");
    Put_Line("  Right button   : `slide' mode  " &
             "   Move   Up/Down : go forward/backwards");
  end if;

  New_Line;
  Put_Line("[Keyboard]");
  Put_Line("  Esc       : exit           " &
           "           Left/Right: turn -or- slide left/right");
  Put_Line("  Alt       : `slide' mode   " &
           "             Up/Down : go forward/backwards");
  Put_Line("  Shift     : `run' mode     " &
           "           PgUp/PgDn : turn up/down");

  -- US: S/D (country-sensitive key image)
  Put_Line("  " & Key_image(31,True) & '/' & Key_image(32,True) &
                "       : swing angle    " &
           "           " & Key_image(30,True) & '/' & Key_image(44,True) &
           "       : slide up/down"); -- US: A/Z (country-sensitive key image)
  Put_Line("  " & Key_image(45,True) &
           "         : snapshot to snap_???.bmp"); -- US: X
  New_Line;

  Menu_Graphics_mode(can_draw, mode);
  if can_draw then
    Demo_3D_01_in_graphics;
    Text_Mode;
  end if;
  if SB_inst_OK then SB_Driver.Uninstall; end if;

exception
  when others =>
    if SB_inst_OK then
      SB_Driver.Uninstall;
    end if;
    raise;
end Demo_3D_01;
