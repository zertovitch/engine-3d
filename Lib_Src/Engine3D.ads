------------------------------------------------------------------------------
--  File:            Engine3D.ads
--  Description:     Basic 3D engine
--  Main reference:  Peroxide tutorial #4, http://www.peroxide.dk/
--
--  Date / Version:  6-Jan-2002; 18-Jul-2001 ; 2-Apr-2001 ; 15-Oct-2000
--  Author:          Gautier de Montmollin
--
--  Copyright (c) Gautier de Montmollin 1999..2002
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

with SVGA;                              use SVGA;
with SVGA.Effects;                      use SVGA.Effects;
with Ada.Unchecked_deallocation;

package Engine_3D is

  subtype Real is SVGA.Effects.Real;

  -- "hand-made" fixed point type for coordinates
  subtype Fix is Integer;     -- no, it isn't an Ada fixed point type
  fix_bits: constant:= 13; -- was 9. 14-Jul-2001: improves rotation quality
  fix_one:  constant:= 2**fix_bits;   -- 1.0 <-> fix_one
  fl_fix_one   : constant Real:= Real(fix_one);
  iv_fl_fix_one: constant Real:= 1.0 / fl_fix_one;

  type Fix_array is array(Natural range <>) of Fix;

  type Point is       record x,y,z: Integer; end record;
  type Fixed_point is record x,y,z: Fix;     end record;

  type ScrPoint is       record x,y: Integer; end record;
  type Fixed_ScrPoint is record x,y: Fix;     end record;

  ------------------------
  -- Vectors & matrices --
  ------------------------
  type Vector3 is array(1..3) of Real;
  subtype RealPoint is Vector3;
  subtype Fixed_vectT is Fixed_point;
  -- for purely moral reasons I prefer to distinguish points and vectors...

  -- Operators & functions in Engine_3D.Math package (see Eng3DMat.ads)

  type Matrix33 is array(1..3,1..3) of Real;
  Id33: constant Matrix33:= ( (1.0,0.0,0.0), (0.0,1.0,0.0), (0.0,0.0,1.0) );

  type FixedMatrix33 is array(1..3,1..3) of Fix;
  FixedId33: constant FixedMatrix33:=
    ( (fix_one,0,0), (0,fix_one,0), (0,0,fix_one) );

  -------------------
  -- Define a face --
  -------------------

  type Index_array is array(Natural range <>) of Natural;

  type Reflexion_type is ( very_matt, matt, neutral, bright, very_bright );
  type Draw_modes is ( textured, colours, invisible, mirror );

  type Object_3D;
  type p_Object_3D is access Object_3D;

  type p_p_Texture_map is access p_Texture_map;

  type Face is record
     P: Index_array(1..4);  -- indices of the edges (anticlockw.)
                  -- one of them can be 0 (triangle); then the
                  -- "missing" edge indicates how to put texture

     draw_mode        : Draw_modes; -- 17-Jul-2001: simplifies portals etc.
     surface_reflexion: Reflexion_type; -- 17-Jul-2001: kind of surface

     -- Portals :
     connecting   : p_Object_3D; -- object behind - if there is one

     -- *** Texture-mapping part:

     texture:       p_p_Texture_map;
                    -- ^ 2-Jan-2002: double-pointing: just change
                    -- `face.texture.all` (also a pointer) to select another
                    -- image, without touching `face.texture` ->
                    -- allows a flexible animated texture management
                    -- e.g. same animation for several faces,
                    -- table of all animated textures, ...
     rep_U, rep_V:  Positive;
     intens_min,
     intens_max  :  Intensity;  -- min: darkest, max: brightest

     -- *** Colour-only part:

     color_min,
     color_max:     Color_type; -- color_min: darkest, color_max: brightest
  end record;

  type Map_idx_pair is record U,V: Natural; end record;
  type Map_idx_pair_array is array(Natural range <>) of Map_idx_pair;

  -- 15-Jul-2001 : things that don't change during the face's life
  type Face_invariants is record
     P_compact   : Index_array(1..4);
                   -- indices of the edges (anticlockw.),
                   -- in compact range : 1..3 for triangle
     last_edge   : Natural;
     UV_extrema  : Map_idx_pair_array(1..4);
                   -- mapping of texture edges according to an eventual
                   -- 0 in P (triangle). Compact range : 1..3 for triangle
     Normal      : Fixed_vectT;
                   -- outer normal vector of unrotated face
  end record;

  -- 17-Jul-2001 : to solve the "nose on portal" problem (black screen there)
  type Proj_status is (
    cannot,            -- nothing to do: off-screen or hidden
    only_behind_face,  -- Z is positive but < Zmin: one can only draw behind
    behind_and_face    -- full rendering: face and object behind.
  );
  type Proj_status_array is array(Natural range <>) of Proj_status;

  subtype Ident is String(1..16); -- for naming things
  type Point_array is array(Natural range <>) of Point;
  type Fixed_point_array is array(Natural range <>) of Fixed_point;
  type Fixed_vect_array is array(Natural range <>) of Fixed_vectT;
  type ScrPoint_array is array(Natural range <>) of ScrPoint;
  type Fixed_ScrPoint_array is array(Natural range <>) of Fixed_ScrPoint;
  type Face_array is array(Natural range <>) of Face;
  type Face_invariants_array is array(Natural range <>) of Face_invariants;
  type Center_array is array(Natural range <>) of Integer;
  type Bool_array is array(Natural range <>) of Boolean;

  Max_faces_per_vertex: constant:= 32;
  type adjacence_table is array(Natural range <>, Natural range <>) of Natural;

  -- 2-Apr-2001: list of objects
  type Object_3D_list;
  type p_Object_3D_list is access Object_3D_list;
  type Object_3D_list is record
    objc: p_Object_3D;
    next: p_Object_3D_list;
  end record;

  -----------------------------------
  -- Now: the Object_3D definition --
  -----------------------------------

  -- 15-Oct-2000: fields Points & Projected are fixed-point for subpixel accuracy

  type Object_3D (Max_points, Max_faces: Integer) is record

    -- The following are data:

    Baseobj    : Point_array(1..Max_points);  -- unmoved, relative edges
    Faces      : Face_array(1..Max_faces);    -- faces

    Num_of_points : Integer:= Max_points;
    Num_of_faces  : Integer:= Max_faces;

    Center        : RealPoint:= (0.0,0.0,0.0);  -- absolute centering
    Auto_rotation : FixedMatrix33:= FixedId33;  -- object orientation
    -- these default values are important for merging!

    -- The following will be calculated

    Points     : Fixed_Point_array(1..Max_points); -- rotated & moved 3D pts
    Projected  : Fixed_ScrPoint_array(1..Max_points);  -- the 2D-screenpoints
    Center_Z   : Center_array(1..Max_faces);     -- Z-val of centres, SORTED
    Order      : Index_array(1..Max_faces);       -- order after sorting
    RotNormals : Fixed_vect_array(1..Max_faces); -- rotated normal vectors

    num_of_adjacents: Index_array(1..Max_points);
    adjacent   : adjacence_table(1..Max_points, 1..Max_faces_per_vertex);

    can_draw_point: Bool_array(1..Max_points);
    can_draw_face : Proj_status_array(1..Max_faces);

    -- The following is calculated at object's initialisation
    Faces_invars : Face_invariants_array(1..Max_faces);

    -- Again data (are here for cache efficiency), defaulted

    Id : Ident:= "-Nameless-      ";   -- Name of the object
    projection_refreshed : Boolean:= False; -- Draw will project when needed
    sorting_refreshed    : Boolean:= False; -- Sometimes, no sorting needed (convex)

    sub_objects: p_Object_3D_list:= null;
    -- List of objects to be drawn AFTER the object itself
    -- e.g., things inside a room -- 2-Apr-2001

  end record; -- Object_3D

  -- Object initialisation (once in object's life)
  procedure Init_object(o: in out Object_3D);
  points_overflow, faces_overflow: exception;

  -- arrays of pointers to objects
  type a_p_Object_3D is array(Integer range <>) of p_Object_3D;

  -------------------------
  -- Projection 3D -> 2D --
  -------------------------

  procedure Project( p3d    :  in Fixed_Point;
                     p2d    : out Fixed_ScrPoint;
                     visible: out Boolean );
    pragma Inline(Project);

  procedure Project( p3d    :  in Point;
                     p2d    : out ScrPoint;
                     visible: out Boolean );
    pragma Inline(Project);

  -- Rotation and projection of whole object.

  --   procedure Rotate_and_Project(o: in out Object_3D);
  --     pragma Inline(Rotate_and_Project);

  --   Commented out 2-Jan-2002: just set o.projection_refreshed
  --   to false, the Draw procedure will do the operation *when needed*
  --   After drawing, the x.projection_refreshed will indicate which x
  --   was drawn and need adjustments, e.g. for animated textures

  -------------
  -- Sorting --
  -------------
  procedure Sort_faces(o: in out object_3D);            -- Z sorting

  ----------------------------
  -- The Object_3D drawing  --
  ----------------------------
  type shading_mode is ( Z_only, Lambert, Gouraud, Phong );

  type surface_select is ( colors_only, textures_only, auto );
    -- auto means: trust the Face.textured (true/false) item.

  type texture_mapping_mode is (
    affine_y_affine_x,  -- everything affine            -> N.z ~= 1 (facing)
    npersp_y_affine_x,  -- ends: persp, H-lines: affine -> N.x ~= 0 (floors)
    affine_y_npersp_x,  -- ends: persp, V-lines: affine -> N.y ~= 0 (walls)
    npersp_y_npersp_x,  -- everything near-perspective (every n pixels)
    auto );             -- auto select according to normal N

  procedure Draw (buffer        : out Screen_Buffer;
                  o             : in out Object_3D;
                  surf_select   : in  surface_select;
                  map_mode      : in  texture_mapping_mode;
                  shading       : in  shading_mode;
                  do_Z_shading  : in  Boolean:= False;
                  minZ, maxZ    : in  Integer:= 0
                 );

  -- N.B.: Z-shading is added to others

  ------------------------------------------------------------------------
  -- Autonomous_Object_3D definition: that object has its own cinematic --
  ------------------------------------------------------------------------

--   type Autonomous_Object_3D is new Object_3D with record
--     Speed, Acceleration: Vector3;
--   end record;

  ---------------
  -- Lightings --
  ---------------

  -- Parallel lights (sun, distant sources,... )
  max_parallel_lights: constant:= 4;
  parallel_lights: Natural:= 0;
  parallel_light_vect:  array(1..max_parallel_lights) of Vector3;
  parallel_light_force: array(1..max_parallel_lights) of Real;

  -- Radial lights (bulb,... )
  max_radial_lights: constant:= 10;
  radial_lights: Natural:= 0;
  radial_light_source: array(1..max_radial_lights) of RealPoint;
  radial_light_force:  array(1..max_radial_lights) of Real;

-- NB: the lights must be rotated (Rotate_lights) !

  procedure Find_light_direction( Pbeg, Pend: Point; dir: out Vector3 );

  -----------------------------
  -- Set engine's parametres --
  -----------------------------

     screen_virtual_size: constant:= 1024;
  fl_screen_virtual_size: constant:= 1024.0;

  -- adimensioned screen size, to lift dependency to resolution
  -- screen_virtual_size represents the height of the screen

  procedure Init_engine(
    X_res, Y_res: Positive;  -- resolution e.g. 640x400

    X_clip_left,
    X_clip_right,
    Y_clip_top,
    Y_clip_bottom: Natural;  -- screen clipping bounds e.g. 0,639,0,399
    Z_clip_min: Real;        -- Z clipping is adimensioned,
                             -- e.g. fl_screen_virtual_size / 10.0
    X_offset,
    Y_offset: Natural;       -- (0,0,0) is projected to (X_offset,Y_offset)
    Focal: Real              -- adimensioned focal length
                             -- e.g. fl_screen_virtual_size
   );

  ----------------
  -- The Camera --
  ----------------

  Eye: RealPoint:= (others=> 0.0); -- position of the eye in the universe...

  World_rotation: Matrix33:= Id33; -- inverse of Camera rotation...

  procedure Rotate_lights;         -- the lightings must follow ! (1x per image)

  -- Free allocated memory:

  procedure Dispose is new Ada.Unchecked_Deallocation(Object_3D,p_Object_3D);

end Engine_3D;
