with Ada.Text_IO, Ada.Integer_Text_IO;

package body Scanline_profiler is
  count: array( Engine_3D.texture_mapping_mode ) of Natural;

  procedure Initialize is
  begin
    count:= (others=> 0);
  end;

  procedure Add_one_count( m: Engine_3D.texture_mapping_mode ) is
  begin
    count( m ):= count( m ) + 1;
  end;

  procedure Finalize is
    use Engine_3D, Ada.Text_IO, Ada.Integer_Text_IO;
    f: File_Type;
    t: Natural:= 0;
  begin
    Create(f, name=> "scanline.log" );
    for m in Engine_3D.texture_mapping_mode loop
      t:= t + count(m);
    end loop;
    if t=0 then
      Put_Line( f, "no time elapsed!" );
    else
      for m in texture_mapping_mode loop
        if m/=auto then
          Put( f, count(m), 10);
          Put( f, (count(m)*100)/t, 10);
          Put( f, "%  for " & texture_mapping_mode'Image( m ) & " : ");
          case m is
            when affine_y_affine_x => Put_Line(f,"facing the face");
            when npersp_y_affine_x => Put_Line(f,"floor");
            when affine_y_npersp_x => Put_Line(f,"wall");
            when npersp_y_npersp_x => Put_Line(f,"general (slowest)");
            when auto => null;
          end case;
        end if;
      end loop;
      Put( f, t, 10); Put_Line(f," -- total");
    end if;
    Close(f);
  end Finalize;

begin
  Initialize;
end Scanline_profiler;
