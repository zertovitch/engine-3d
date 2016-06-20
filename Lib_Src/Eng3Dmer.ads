------------------------------------------------------------------------------
--  File:            Eng3Dmer.ads
--  Description:     Basic 3D engine - merging
--  Date / Version:  12.IV.1999
------------------------------------------------------------------------------

package Engine_3D.Merging is

  ------------------------------
  -- Merging objects into one --
  ------------------------------

  function Merge( oa: a_p_Object_3D ) return p_Object_3D;
  function "+"( o1, o2: p_Object_3D ) return p_Object_3D;
  function "+"( o: p_Object_3D; oa: a_p_Object_3D ) return p_Object_3D;
  function "+"( oa: a_p_Object_3D; o: p_Object_3D ) return p_Object_3D;

end Engine_3D.Merging;
