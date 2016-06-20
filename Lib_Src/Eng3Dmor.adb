with Engine_3D.Math;                    use Engine_3D.Math;

package body Engine_3D.Morphing is

 -- Translation:
  function "+"( o: Object_3D; direction: Vector3 ) return p_Object_3D is
    niou: constant p_Object_3D:= new Object_3D'(o); -- just copy everything!
  begin
    for i in 1..o.num_of_points loop -- modifiy points coordinates
      niou.baseobj(i):= To_Point( To_RealPoint(o.baseobj(i)) + direction );
    end loop;
    return niou;
  end "+";

  function "+"( direction: Vector3; o: Object_3D ) return p_Object_3D is
  begin
    return o + direction;
  end "+";

  function "+"( o: p_Object_3D; direction: Vector3 ) return p_Object_3D is
  begin
    return o.all + direction;
  end "+";

  function "+"( direction: Vector3; o: p_Object_3D ) return p_Object_3D is
  begin
    return o.all + direction;
  end "+";

 -- Homothethy:
  function "*"( o: Object_3D; factor: Real ) return p_Object_3D is
    niou: constant p_Object_3D:= new Object_3D'(o); -- just copy everything!
  begin
    for i in 1..o.num_of_points loop -- modifiy points coordinates
      niou.baseobj(i):= To_Point( factor * To_RealPoint(o.baseobj(i)) );
    end loop;
    return niou;
  end "*";

  function "*"( factor: Real; o: Object_3D ) return p_Object_3D is
  begin
    return o * factor;
  end "*";

  function "*"( o: p_Object_3D; factor: Real ) return p_Object_3D is
  begin
    return o.all * factor;
  end "*";

  function "*"( factor: Real; o: p_Object_3D ) return p_Object_3D is
  begin
    return o.all * factor;
  end "*";

end Engine_3D.Morphing;
