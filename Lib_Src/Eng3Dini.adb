with Engine_3D.Math;                    use Engine_3D.Math;
with Engine_3D.Options;
with Ada.Exceptions;                    use Ada.Exceptions;

-- 14-Jul-2001 : detached from Engine_3D source
--
-- We are here in a calm area, the world before any display.
--
-- We can afford to be pedantic, double-checking, find bugs in objects
-- so that they will be displayed correctly.

package body Engine_3D.Initialisations is

  bad_vertex_number, duplicated_vertex,
    duplicated_vertex_location: exception;
  point_unmatched, too_many_adjacences: exception;
  zero_normal: exception;
  zero_summed_normal: exception;
  zero_averaged_normal: exception;

  function Coords( p: Point ) return String is
  begin
    return '(' & Integer'Image(p.x) &
           ',' & Integer'Image(p.y) &
           ',' & Integer'Image(p.z) &
           ')';
  end Coords;

  procedure Init_object(o: in out Object_3D) is
    N: Vector3;
    laengde : Real;

    procedure Check_faces is

      procedure Check(f,v: Integer) is
      begin
        if v < 0 or v > o.num_of_points then
          raise_exception(bad_vertex_number'Identity,
               o.id & " face="   & Integer'Image(f) &
                      " vertex=" & Integer'Image(v));
        end if;
      end Check;

      procedure Check_duplicate(f,Pn1,Pn2: Integer) is
      begin
        -- Skip "dead" edge (triangle), 30-Dec-2001
        if Pn1=0 or Pn2=0 then return; end if;
        -- Detect same point number
        if Pn1=Pn2 then
          raise_exception(duplicated_vertex'Identity,
               o.id & " in face "   & Integer'Image(f) );
        end if;
        -- Detect same point coordinates (tolerated in an object,
        -- although inefficient, but harms as vertex of the same face!)
        if o.baseobj(Pn1) = o.baseobj(Pn2) then
          raise_exception(duplicated_vertex_location'Identity,
               o.id & " in face "   & Integer'Image(f) );
        end if;
      end Check_duplicate;

    begin
      for fa in 1 .. o.num_of_faces loop
        for edge_num in 1..4 loop
          Check( fa, o.faces(fa).P(edge_num) );
          -- 16-Jul-2001
          for other_edge in edge_num+1 .. 4 loop
            Check_duplicate( fa, o.faces(fa).P(edge_num),
                                 o.faces(fa).P(other_edge) );
          end loop;
        end loop;
      end loop; -- fa
    end Check_faces;

    procedure Find_adjacences is -- for PointNormal
      AntalHits: Integer;

    begin
      for pt in reverse 1..o.num_of_points loop
        AntalHits:= 0;
        for fa in reverse 1 .. o.num_of_faces loop
          declare the_face: Face renames o.faces(fa);
          begin
           if the_face.P(1) = pt or else the_face.P(2) = pt or else
              the_face.P(3) = pt or else the_face.P(4) = pt
           then
             AntalHits:= AntalHits + 1;
             if AntalHits > Max_faces_per_vertex then
              raise_exception(too_many_adjacences'Identity,
               o.id & " pt="&Integer'Image(pt));
             end if;
             o.adjacent(pt,AntalHits):= fa;
           end if; -- in which faces does the point appear
          end;
        end loop; -- fa
        if AntalHits=0 then
         raise_exception(point_unmatched'Identity,
          o.id & " pt="&Integer'Image(pt));
        end if;
        o.num_of_adjacents(pt):= AntalHits;
      end loop; -- pt
    end Find_adjacences;

    procedure Add_Normal_of_3p(Pn0,Pn1,Pn2: Integer; N: in out Vector3) is
      P0,P1,P2, N_contrib: Vector3;
    begin
      if Pn0=0 or else Pn1=0 or else Pn2=0 then return; end if;
      P0:= To_RealPoint(o.baseobj(Pn0));
      P1:= To_RealPoint(o.baseobj(Pn1));
      P2:= To_RealPoint(o.baseobj(Pn2));
      N_contrib:= (P1-P0)*(P2-P0) ;
      if Norm(N_contrib) = 0.0 then  -- 16-Jul-2001
        raise_exception( zero_normal'Identity,
               o.id &
               " Pn0=" & Integer'Image(Pn0) &
               "P0=" & Coords(o.baseobj(Pn0)) &
               " Pn1=" & Integer'Image(Pn1) &
               "P1=" & Coords(o.baseobj(Pn1)) &
               " Pn2=" & Integer'Image(Pn2) &
               "P2=" & Coords(o.baseobj(Pn2)) &
               "Nc=(" & Real'Image(N_contrib(1)) &
                  ',' & Real'Image(N_contrib(2)) &
                  ',' & Real'Image(N_contrib(3)) & ") "
               );
      end if;
      N:= N + N_contrib;
    end Add_Normal_of_3p;

    -- 15-Jul-2001
    procedure Calculate_invariants( fa: in Face; fi: out Face_invariants ) is
      l: Natural:= 0;
      quadri_edge:  array(fa.P'Range) of Natural;
      maxx, maxy: Natural;
    begin
      l:= 0;
      for qe in fa.P'Range loop
        if fa.P(qe) /= 0 then
          l:= l + 1;
          quadri_edge(l):= qe; -- if triangle, "map" edge on a quadri
          fi.P_compact(l):= fa.P(qe);
        end if;
      end loop;

      fi.last_edge:= l;

      -- * Face invariant : Textured face: extremities
      if fa.texture /= null then
       maxx:= (2 ** fa.texture.all.x_bits)  -1;
       maxy:= (2 ** fa.texture.all.y_bits)  -1;

       for e in  1..l loop
         -- 20.VIII.1999: texture y=0 top, y=maxy bottom like screen bitmap !
        case quadri_edge(e) is
          when 1=> fi.UV_extrema(e):= (0,maxy);    -- bottom, left    4--<--3
          when 2=> fi.UV_extrema(e):= (maxx,maxy); -- bottom, right   |     |
          when 3=> fi.UV_extrema(e):= (maxx,0);    -- top, right      1-->--2
          when 4=> fi.UV_extrema(e):= (0,0);       -- top, left
          when others => null;
        end case;
       end loop;
      end if;

      -- * Face invariant : Normal of unrotated face

      N:= (0.0,0.0,0.0);

      Add_Normal_of_3p( fa.P(1), fa.P(2), fa.P(4), N);
      -- 5.IV.1999: we sum other normals for not too flat faces
      Add_Normal_of_3p( fa.P(2), fa.P(3), fa.P(1), N);
      Add_Normal_of_3p( fa.P(3), fa.P(4), fa.P(2), N);
      Add_Normal_of_3p( fa.P(4), fa.P(1), fa.P(3), N);
      -- NB: it also fits for triangles !

      laengde:= Norm( N );

      if laengde = 0.0 then raise zero_summed_normal; end if; -- 16-Jul-2001

      fi.normal.X := fix((N(1)/laengde) * fl_fix_one);
      fi.normal.Y := fix((N(2)/laengde) * fl_fix_one);
      fi.normal.Z := fix((N(3)/laengde) * fl_fix_one);

    end Calculate_invariants;

    -- 16-Jul-2001
    procedure Check_averaged_normal(nr: Integer) is
      sum: Fixed_vectT:= (0,0,0);
      result: Vector3;
    begin
      for i in reverse 1 .. o.num_of_adjacents(nr) loop
        sum:= sum + o.Faces_invars( o.adjacent(nr,i) ).Normal;
      end loop;

      result:= To_Vector3( sum );
      if Norm( result )=0.0 then
        raise_exception( zero_averaged_normal'Identity,
         o.id & " for point" & Integer'Image(nr));
      end if;
    end Check_averaged_normal;

  begin
    -- Some checks:
    if o.Num_of_points > o.Max_points then raise points_overflow; end if;
    if o.Num_of_faces  > o.Max_faces  then raise faces_overflow;  end if;

    Check_faces;

    Find_adjacences;

    for i in reverse 1..o.Num_of_faces loop
      begin
        Calculate_invariants( o.Faces(i), o.Faces_invars(i) );
      exception
        when zero_summed_normal =>
              raise_exception( zero_summed_normal'Identity,
               o.id & " face="&Integer'Image(i));
      end;
    end loop;

    for counter in o.Order'Range loop
      o.Order(counter):= counter;
    end loop;

    -- 16-Jul-2001
    if Engine_3D.Options.check_zero_averaged_normals then
      for i in 1..o.Num_of_points loop
        Check_averaged_normal( i );
      end loop;
    end if;

  end Init_object;

end Engine_3D.Initialisations;
