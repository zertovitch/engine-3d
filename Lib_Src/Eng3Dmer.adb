with Engine_3D.Math;                    use Engine_3D.Math;

package body Engine_3D.Merging is

function Merge( oa: a_p_Object_3D ) return p_Object_3D is
  np, nf, p, f: Natural:= 0;
  begin
   for i in oa'Range loop
     np:= np + oa(i).num_of_points;
     nf:= nf + oa(i).num_of_faces;
   end loop;
   declare
     m: constant p_Object_3D:= new Object_3D(np,nf);
   begin
     for i in oa'Range loop
      declare oi: Object_3D renames oa(i).all;
      begin
       for pi in 1..oi.num_of_points loop -- copy points
         m.baseobj(p+pi):=
           To_Point(
            To_real(oi.auto_rotation) * To_RealPoint(oi.baseobj(pi)) + oi.center
           );
       end loop;
       for fi in 1..oi.num_of_faces loop -- copy faces
         m.faces(f+fi):= oi.faces(fi);
         for sommet in m.faces(f+fi).p'Range loop -- in fact, hardcoded 1..4
           if oi.faces(fi).p(sommet) > 0 then
             m.faces(f+fi).p(sommet):= oi.faces(fi).p(sommet) + p;
           else
             m.faces(f+fi).p(sommet):= 0; -- a "missing" edge must keep 0
           end if;
         end loop;
       end loop;
       p:= p + oi.num_of_points;
       f:= f + oi.num_of_faces;
      end;
     end loop; -- i
     Init_Object(m.all);
     return m;
   end;
  end;

function "+"( o1, o2: p_Object_3D ) return p_Object_3D is
  begin return Merge((o1,o2)); end;

function "+"( o: p_Object_3D; oa: a_p_Object_3D ) return p_Object_3D is
  begin return o + Merge(oa); end;

function "+"( oa: a_p_Object_3D; o: p_Object_3D ) return p_Object_3D is
  begin return Merge(oa) + o; end;

end Engine_3D.Merging;
