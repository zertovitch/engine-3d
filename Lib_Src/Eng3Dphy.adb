-----------------------------------------------------------------------------
--  File: eng3dphy.adb; see specification (eng3dphy.ads)
-----------------------------------------------------------------------------

with Engine_3D.Math;                    use Engine_3D.Math;

package body Engine_3D.Physics is

  function Inside_convex( p: RealPoint; o: Object_3D ) return Boolean is
    dot: Real;
    p_to_vertex: Vector3;
    j1: Positive;
  begin
    -- !! avec rotation !!
    for face in reverse 1..o.Num_of_faces loop
      j1:= o.Faces_invars(face).P_compact(1);
      p_to_vertex:= p - To_RealPoint(o.Points(j1));
      dot:= To_Vector3(o.RotNormals(face)) * p_to_vertex;
      if dot < 0.0 then return False; end if;
    end loop;
    return True;
  end Inside_convex;

  -- 2-Jan-2002: reactions.

  -- Internal: against object's faces or against an ellipsoid

  procedure Reaction_against_faces(
    P0          : RealPoint;      -- Starting point
    radius      : Real;
    step        : in out Vector3; -- Whole step (in: desired, out: effective)
    o           : Object_3D;
    method      : Reaction_method;
    reacted     : out Float;      -- in proportion to step
    landed_into : out p_Object_3D -- eventual entered connecting object
  )
   is
    P0rf,P1rf: RealPoint;
    u,n : Vector3;
    dist: Real; -- distance orientee
    retour: Real:= 0.0;
    j1: Positive;
    lstep0: constant Real:= Norm(step);

    function Dans_prisme_epaissi(f: Positive) return Boolean is
      sfp1: Positive;
      js, jsp1: Positive;
      Ps, Psp1: RealPoint;
      u,a,npa: Vector3;
      dsp: Real;
      facteur: constant:= 1.1;
    begin
      for sf in reverse 1..o.Faces_invars(f).last_edge loop
        sfp1:= 1 + sf mod o.Faces_invars(f).last_edge;
        js  := o.Faces_invars(f).P_compact(sf);
        jsp1:= o.Faces_invars(f).P_compact(sfp1);
        Ps  := To_RealPoint(o.BaseObj(js));
        Psp1:= To_RealPoint(o.BaseObj(jsp1));
        a:= Psp1 - Ps; -- vecteur arete
         -- npa: ortho a la face num. sf du prisme engendre
         --      par la face de l'objet et la normale au plan
        npa:= n*a; -- npa vers interieur du prisme
        npa:= 1.0/Norm(npa) * npa;
        u:= P1rf - (To_RealPoint(o.BaseObj(js))+o.Center);
        dsp:= u * npa;
        if dsp < -radius * facteur then
          return False;
        end if;
      end loop;
      return True;
    end Dans_prisme_epaissi;

  begin
    reacted:= 0.0;
    if lstep0 < Real'Epsilon then return; end if;

    -- Portails !!

    -- Referentiel absolu xxx "camera"
    P0rf:= P0;
    -- World_rotation * (P0-Eye);     -- En ref. "camera" ca ne marche pas
    -- step:= World_rotation * step;  -- sauf pour qqes angles ?!

    P1rf:= P0rf + step;
    for face in reverse 1..o.Num_of_faces loop
      n:= To_Vector3(o.Faces_invars(face).normal); -- o.RotNormals(face)
      if step * n < 0.0 then
        j1:= o.Faces_invars(face).P_compact(1);
        -- !! ref absolu: auto_rotation manque (mais c'est bien ainsi).
        u:= P1rf - (To_RealPoint(o.BaseObj(j1))+o.Center);
        dist:= u * n;
        if dist < radius and then -- includes negatives values!
           Dans_prisme_epaissi(face) then
          if o.Faces(face).connecting /= null then
            null; -- on s'occupe des portails ailleurs,
                  -- dans l'ordre de traversee suivant la trajectoire
            -- !! a redefinir: comportement portails indep de traversable
            -- ou non. Ajout dans face: .traversable !!
          else
            reacted:= reacted + Float(retour / lstep0);
            -- !! tol en const
            case method is
              when elastic => null; -- a faire !!
              when slide =>
                retour:= radius - dist;
                step:= step + retour * n;
                -- Comme step * n < 0.0 et radius-dist > 0.0
                -- et en plus dist(P0,face) > radius   -hypothese-
                -- ||step_nouv|| < ||step_ancien|| algo decroissant :-)
            end case;
          end if;
        end if;
      end if;
    end loop;
    -- step:= Transpose(World_rotation) * step;
    -- !! sous-objets
  end Reaction_against_faces;

  procedure Reaction_against_ellipsoid(
    P0          : RealPoint;      -- Starting point
    radius      : Real;
    step        : in out Vector3; -- Whole step (in: desired, out: effective)
    o           : Object_3D;
    method      : Reaction_method;
    reacted     : out Float;      -- in proportion to step
    landed_into : out p_Object_3D -- eventual entered connecting object
  )
   is
  begin
    null; -- not done!!
  end Reaction_against_ellipsoid;

  procedure Reaction(
    P0          : RealPoint;      -- Starting point
    radius      : Real;
    step        : in out Vector3; -- Whole step (in: desired, out: effective)
    o           : Object_3D;
    method      : Reaction_method;
    reacted     : out Float;      -- in proportion to step
    landed_into : out p_Object_3D -- eventual entered connecting object
  )
   is
  begin
    landed_into:= null;
    -- !! choix ellipsoide
    Reaction_against_faces(P0,radius,step,o,method,reacted,landed_into);
  end Reaction;

end Engine_3D.Physics;
