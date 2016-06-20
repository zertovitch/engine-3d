------------------------------------------------------------------------------
--  File:            SVEffBum.ads
--  Description:     - Bump mapping on textures
--
--  Date / Version:  4-Jan-2002 ; 18-Dec-2001
--  Author:          Gautier de Montmollin
--
--  Copyright (c) Gautier de Montmollin 2001
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------
package SVGA.Effects.Bump is

  -- Bump map definitions --

  type Height_Map (New_Width, New_Height : Natural) is limited private;
  type Height_Map_access is access Height_Map;

  type Boundary_mode is (zero, periodic);

  ----------
  -- Blob --
  ----------

  -- Put a "blob" into the height map.

  procedure Blob(map        : in out Height_Map;
                 mx,my      : Natural;
                 radius     : Positive;
                 depth      : Integer;
                 boundary   : Boundary_mode );

  ------------
  -- Evolve --
  ------------

  -- Make a pair of height maps ( representing times t_{k-1}, t_k ) evolve
  -- into a t_k, t_{k+1} pair, according to a finite difference scheme.
  -- The map_{k-1} becomes map_{k+1}.

  -- Time step:    k-1 -----\
  --                         <Evolve>-----> k+1
  --               k   -----/

  type Propagation_mode is (heat, wave);

  procedure Evolve( map_k, map_kp1: in out Height_Map;
                    boundary      : Boundary_mode;
                    propagation   : Propagation_mode );

  -----------
  -- Apply --
  -----------

  -- Apply a height map to an image (original) and create a deformed
  -- image (deformed). The main colour-shift map (main_intensity_map)
  -- is also used for the effect.

  procedure Apply(bump_map :        Height_Map;
                  light_x  :        Integer;
                  light_y  :        Integer;
                  original :        Screen_Buffer;
                  deformed : in out Screen_Buffer
                 );

  procedure Apply(bump_map :        Height_Map;
                  light_x  :        Integer;
                  light_y  :        Integer;
                  original :        Texture_map;
                  deformed : in out Texture_map
                 );

  Height_Map_Error: exception;

private
   type Height_Buffer is array (Natural range <>) of Integer;
   type Height_Buffer_Access is access all Height_Buffer;

   procedure Dispose is
     new Ada.Unchecked_Deallocation (Height_Buffer, Height_Buffer_Access);

   type Height_Map (New_Width, New_Height : Natural) is
     new Ada.Finalization.Limited_Controlled with
      record
         Width  : Natural := New_Width;
         Height : Natural := New_Height;
         Data   : aliased Height_Buffer_Access;
      end record;

   procedure Finalize (HM : in out Height_Map);
   procedure Initialize (HM : in out Height_Map);

end SVGA.Effects.Bump;
