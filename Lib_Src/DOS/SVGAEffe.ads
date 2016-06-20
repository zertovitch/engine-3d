------------------------------------------------------------------------------
--  File:            SVGAEffe.ads
--  Description:     - Definition of textures, colour shifts
--                   - Horizontal and vertical scanlines with special
--                     shading and texture effects (for 3D)
--
--  Date / Version:  15-May-2002 ; ... ; 11-Aug-2001 ; ... ; 3.IV.1999
--  Author:          Gautier de Montmollin
--
--  Copyright (c) Gautier de Montmollin 1999 .. 2002
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------
--  27-Jul-2001: Transparency colour becomes a constant
--   8-Aug-2001: Degrades may be larger than 0..Intensity'last (no reason to
--               restrict)
--  11-Aug-2001: sfix_bits=10 instead of 8

with Ada.Finalization, Ada.Numerics.Generic_Elementary_functions, Interfaces;

package SVGA.Effects is

  --------------
  -- Clipping --
  --------------

  -- NB: 1) these variables must be set !
  --     2) Hor_lines do only X-clipping; Y-clipping is meant
  --        to be done at polygon scanning level. Idem for Y-X

  XLeftClip, XRightClip: X_loc;  -- horizontal clipping range
  YTopClip,  YBotClip:   Y_loc;  -- vertical clipping range

  -------------------------------
  -- Texture, intensity, Phong --
  -------------------------------

  -- Texture maps --
  type Texture_map(x_bits, y_bits: Natural) is limited private;

  -- You must use these procedures to set a texture's contents...
  procedure Set_Pixel(t: in out Texture_map; x,y: Natural; Color: Color_Type);
  procedure Put_Buffer (Source      : in     Screen_Buffer;
                        Destination : in out Texture_map);

  -- Pointers on texture maps:
  type p_Texture_map is access all Texture_map;

  -- The key colour number set to the following constant will
  -- be interpretated as transparent in a texture map:
  transparency_color: constant:= 255;

  -- Some famous values: DOOM: 247, Duke3D: 255

  -- Intensity of colours --
  subtype Intensity is Integer range -31 .. 32;
  -- 0: neutral; >0: brighter <0: darker
  -- NB: in a RGB "truecolor" context it can tend more in R,G or B...

  -- Intensity mapping --
  --  This is for defining intensities in a poor
  --  256-colour video mode with a palette

  type Color_degrade_description is record
     c: Color_type; -- 1st colour index in the d\'egrad\'e
     n: Integer;    -- |n|+1= number of colours in the d\'egrad\'e
  end record;       -- n>0 increasing intensity; n<0 decreasing intensity

  type Degrade_description is array(Natural range <>) of
         Color_degrade_description;

  -- Some famous mappings...

  Doom_intensities: constant Degrade_description:=
   ( (5,-3),    (16,-15), (32,-15), (48,-15),
     (64,-15),  (80,-15), (96,-15), (112,-15), (128,-15),
     (144,-7),  (152,-7), (160,-7), (168,-23), (192,-15),
     (208,-15), (224,-7), (232,-3), (236,-3), (240,-6),
     (250,-4) );

  Duke3D_intensities: constant Degrade_description:=
   ( (0,31),   (32,31),  (64,15),  (80,15),  (96,31),  (128,15),
     (144,15), (160,31), (192,15), (208,15), (224,15) );

  type Intensity_map is array( Color_type, Intensity ) of Color_type;

  -- There is a main intensity map.
  procedure Reset_main_intensity_map;
  procedure Calculate_main_intensity_map(d: degrade_description);
  function Get_main_intensity_map return Intensity_map; -- 2-Aug-2001

  -- Phong map (indices only - the contents are handled by SVGA.Effects) --
  Phong_max_idx: constant:= 255;
  subtype Phong_index is Natural range 0..Phong_max_idx;

  -- Floating point type used here
  subtype Real is Long_Float;
  package El_Func is new Ada.Numerics.Generic_Elementary_functions(Real);
  use El_Func;

  -- "hand-made" fixed point type for subpixel/subtexel positions
  subtype  Sfix is Interfaces.Integer_32;
  -- unsigned couterpart:
  subtype uSfix is Interfaces.Unsigned_32;
  -- bits after point:
  sfix_bits: constant:= 10;
  sfix_one:  constant:= 2**sfix_bits;       -- 1.0 <-> sfix_one
  fl_sfix_one:    constant Real:= Real(sfix_one);
  iv_fl_sfix_one: constant Real:= 1.0 / fl_sfix_one;

  ------------------------------------------------
  -- The special Hor_Line / Ver_Line procedures --
  ------------------------------------------------

  -- NB:  X1<X2 and Y1<Y2 are required (no line in ">=" case)

  -- *** A) Shaded colours ***

  -- Coulour value is interpolated between C1 and C2 :

  procedure Gouraud_Hor_Line
     (Buffer : in out Screen_Buffer;
      X1, X2 : in     Integer;
      Y      : in     Y_Loc;
      C1, C2 : in     Color_Type);

  -- Environment mapping (pseudo-Phong):

  procedure EnvMap_Hor_Line
     (Buffer              : in out Screen_Buffer;
      X1, X2              : in     Integer;
      Y                   : in     Y_Loc;
      U1, V1, U2, V2      : in     Phong_index;
      Cmin, Num_of_shades : in     Color_type);

  -- *** B) Shaded textures ***

  procedure Set_Current_texture
     (Texture      : in Texture_map;
      rep_U, rep_V : in Positive );   -- texture can be repeated

  -- Texture is linearily mapped (faster but distorted if Z varies) :

  procedure Affine_TextureMap_Hor_Line
     (Buffer         : in out Screen_Buffer;
      X1, X2         : in     Integer;
      Y              : in     Y_Loc;
      U1, V1, U2, V2 : in     Sfix;     -- end points in texture (fxd-pnt)
      int1, int2     : in     Intensity -- intensity at end points
     );

  procedure Affine_TextureMap_Ver_Line
     (Buffer         : in out Screen_Buffer;
      X              : in     X_Loc;
      Y1, Y2         : in     Integer;
      U1, V1, U2, V2 : in     Sfix;     -- end points in texture (fxd-pnt)
      int1, int2     : in     Intensity -- intensity at end points
     );

  -- Texture is mapped according to perspective :

  procedure TextureMap_Hor_Line
     (Buffer             : in out Screen_Buffer;
      X1, X2             : in     Integer;
      Y                  : in     Y_Loc;
      UP1, VP1, UP2, VP2 : in     Real;         -- end 1/points in texture
      iv_Z1, iv_Z2       : in     Real;         -- end 1/points' Zs
      int1, int2         : in     Intensity     -- intensity at end points
     );

private

   type Texture_map(x_bits, y_bits: Natural) is
     new Ada.Finalization.Limited_Controlled with
      record
        width, height: Natural;
        U_mask, V_mask: Interfaces.Unsigned_32;
        Data: aliased Data_Buffer_Access;
      end record;

   -- 11-Aug-2001: a bit of explanation. Here are 32-bits unsigned
   --
   -- r/s= repetition of texture, U/V= integral coord,
   --   p= fixed-point precision
   --
   -- Example for a 2**8 x 2**7 bitmap, to be repeated up to 2**3
   -- times horizontaly and 2**4 vertically, with 10-bits subtexel precision
   --
   -- For U:    [-----------rrrUUUUUUUUpppppppppp]
   --                         3       8        10
   -- For V:    [---rrrrVVVVVVVpppppppppp--------]
   --                  4     7         10       8
   --
   -- After "and" with U_mask and V_mask masks, merge with "or"
   -- and a shift_right of sfix_bits, we have exactly the coord
   -- of texture data pixel:
   --
   --           [-----------------VVVVVVVUUUUUUUU]

   procedure Finalize (TM : in out Texture_map);
   procedure Initialize (TM : in out Texture_map);

  main_intensity_map: Intensity_map;
  -- NB: for RGB "true-color" it might be a good idea to have
  -- a function instead of an array, or to round colour value...


end SVGA.Effects;
