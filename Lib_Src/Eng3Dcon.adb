package body Engine_3D.Construct is

  procedure Find_or_create( p: Point; o: in out Object_3D; idx: out Natural) is
  begin
    for i in 1 .. o.Num_of_points loop
      if o.BaseObj(i) = p then  --  Point already exists
        idx:= i;
        return;
      end if;
    end loop;
    o.Num_of_points:= o.Num_of_points + 1;
    if o.Num_of_points > o.Max_points then
      raise Total_points_exhausted;
    end if;
    o.BaseObj(o.Num_of_points):= p;
    idx:= o.Num_of_points;
  end Find_or_create;

  procedure Create_face( f: Face;
                         p: Point_array;   -- imposed coordinates of edges
                         e: Bool_array;    -- what is edge or not (triangles)
                         o: in out Object_3D) is
    idx: array(p'Range) of Natural;
    nf: Face:= f;
  begin
    for i in idx'Range loop
      if e( i - idx'First + e'First ) then
        Find_or_create( p(i), o, idx(i) );
      else
        idx(i):= 0;
      end if;
    end loop;

    o.Num_of_faces:= o.Num_of_faces + 1;
    if o.Num_of_faces > o.Max_faces then
      raise Total_faces_exhausted;
    end if;
    for i in idx'Range loop
      nf.P( i - idx'First + 1 ):= idx(i);
    end loop;
    o.Faces(o.Num_of_faces):= nf;
  end Create_face;

end Engine_3D.Construct;
