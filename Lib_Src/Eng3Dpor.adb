package body Engine_3D.Portals is

  red   : constant:= 248; -- !! should go in game_colors
  green : constant:= 251;

  -- [a] Cheap but fast portal method with rectangles.

  procedure Intersect (A,B: Rectangle; C: out Rectangle; non_empty: out Boolean) is
  begin
    C.X1:= Integer'Max(A.X1,B.X1);
    C.X2:= Integer'Min(A.X2,B.X2);
    C.Y1:= Integer'Max(A.Y1,B.Y1);
    C.Y2:= Integer'Min(A.Y2,B.Y2);
    non_empty:= C.X1 <= C.X2 and C.Y1 <= C.Y2;
  end Intersect;

  procedure Find_bounding_box(o: Object_3D; face: Natural; b: out Rectangle) is
    sp: Fixed_ScrPoint;
  begin
    b:= ( X1|Y1=> Integer'Last, X2|Y2=> Integer'First );

    for sf in reverse 1 .. o.Faces_invars(face).last_edge loop
      sp:= o.Projected( o.Faces_invars(face).P_compact(sf) );
      b.X1:= Integer'Min(b.X1, sp.x/fix_one);
      b.X2:= Integer'Max(b.X2, sp.x/fix_one);
      b.Y1:= Integer'Min(b.Y1, sp.y/fix_one);
      b.Y2:= Integer'Max(b.Y2, sp.y/fix_one);
    end loop;
  end Find_bounding_box;

  procedure Draw_boundary( buffer: out Screen_Buffer; clip: Rectangle ) is
  begin
    Frame_Rect( buffer, clip.x1,  clip.y1,  clip.x2,  clip.y2,   green );
    Line( buffer, clip.x1,clip.y1,clip.x2,clip.y2, red );
    Line( buffer, clip.x2,clip.y1,clip.x1,clip.y2, red );
  end Draw_boundary;

end Engine_3D.Portals;
