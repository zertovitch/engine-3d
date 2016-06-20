with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with SVGA;                              use SVGA;
with GNAT.OS_Lib;

procedure Menu_Graphics_mode(can_draw: out Boolean;
                             mode    : out SVGA_mode) is
  nres: Integer;
  force_banked: Boolean:= False;
    -- the VESA graphics do work under NT, but banked mode, not linear mode
  ams: Video_Mode_Set;
begin
  Put_Line("[VESA Graphics mode]");
  declare
    OS: constant String:= GNAT.OS_Lib.GetEnv( "OS" ).all;
  begin
    Put_Line("   Operating System is (declared as) : [" & OS & "].");
    if OS = "Windows_NT" then
      Put_Line("   NB: Under this system (" & OS &
               ") we must force banked mode.");
      force_banked:= True;
    end if;
  end;
  New_Line;

  begin
    ams:= Get_available_modes;
  exception
    when others =>
      Put_Line("VBE error : maybe VESA not available");
      can_draw:= False;
      return;
  end;
  for i in ams'Range loop
    Put("     "); Put(Video_mode'Pos(i)+1,0);
    Put(")   ");
    if ams(i) then
      Put("available ----:---> ");
    else
      Put("NOT available :     ");
    end if;
    Put_Line(Video_mode'Image(i));
  end loop;

  begin
     -- Switch to graphics mode
    can_draw:= True;
    Put("Choice: "); Get(nres); Skip_Line; New_Line;
    case nres is
      when      1 => mode:= M640x400;
      when      2 => mode:= M640x480;
      when      3 => mode:= M800x600;
      when      4 => mode:= M1024x768;
      when      5 => mode:= M1280x1024;
      when      6 => mode:= M1600x1200;
      when others => Put_Line("Bad choice...");
                     can_draw:= False;
    end case;
    if nres in 1..6 then
      Graphics_Mode(mode, force_banked, use_own_font => True);
    end if;
  exception
    when Data_error =>
      Skip_Line; Put_Line("Valid number, please...");
      can_draw:= False;
    when others =>
      Put_Line("Unsupported display type, or something else...");
      can_draw:= False;
  end;
end Menu_Graphics_mode;
