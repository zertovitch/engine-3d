------------------------------------------------------------------------------
--  File:            Meteor.ads
--  Description:     Meteor 3D model.
--                   Copyright (c) Gautier de Montmollin 1999
--  Date / Version:  (separated from game) 7-Jan-2002
------------------------------------------------------------------------------

with Engine_3D;

package Meteor is

  meteor01: Engine_3D.p_Object_3D;

  procedure Init(meteor_texture: Engine_3D.p_p_Texture_map);

end Meteor;
