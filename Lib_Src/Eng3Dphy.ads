------------------------------------------------------------------------------
--  File:            eng3dphy.ads
--  Description:     Basic 3D engine - Geometry, cinematic, mechanics
--  Date / Version:  13-Jan-2002 ; 14-Dec-1999
--  Author:          Gautier de Montmollin
--
--  Copyright (c) Gautier de Montmollin 1999..2002
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

package Engine_3D.Physics is

  -- Is p inside o (supposed convex) ?

  function Inside_convex( p: RealPoint; o: Object_3D ) return Boolean;

  -- Reaction to an object - and the world connected to it

  type Reaction_method is ( elastic, slide );

  procedure Reaction(
    P0          : RealPoint;      -- Starting point
    radius      : Real;
    step        : in out Vector3; -- Whole step (in: desired, out: effective)
    o           : Object_3D;
    method      : Reaction_method;
    reacted     : out Float;      -- in proportion to step
    landed_into : out p_Object_3D -- eventual entered connecting object
  );

end Engine_3D.Physics;
