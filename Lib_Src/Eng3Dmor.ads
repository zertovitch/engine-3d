------------------------------------------------------------------------------
--  File:            Eng3Dmer.ads
--  Description:     Basic 3D engine - morphing
--  Date / Version:  31.VII.1999
------------------------------------------------------------------------------

package Engine_3D.Morphing is

  ------------------------------------------------------
  -- Morph an object and save contents into a new one --
  ------------------------------------------------------

 -- Translation:
  function "+"( o: Object_3D; direction: Vector3 ) return p_Object_3D;
  function "+"( direction: Vector3; o: Object_3D ) return p_Object_3D;
  function "+"( o: p_Object_3D; direction: Vector3 ) return p_Object_3D;
  function "+"( direction: Vector3; o: p_Object_3D ) return p_Object_3D;

 -- Homothethy:
  function "*"( o: Object_3D; factor: Real ) return p_Object_3D;
  function "*"( factor: Real; o: Object_3D ) return p_Object_3D;
  function "*"( o: p_Object_3D; factor: Real ) return p_Object_3D;
  function "*"( factor: Real; o: p_Object_3D ) return p_Object_3D;

end Engine_3D.Morphing;
