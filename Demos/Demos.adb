with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;

with Demo_3D_00, Demo_3D_01;
--  Of course you can also build them separately as main programs

procedure Demos is
  n:  Integer;
  ok: Boolean;
begin
  loop
   Put_Line("  +--------------------------+");
   Put_Line("  |  Shell for the 3D demos  |");
   Put_Line("  +--------------------------+");
   New_Line;
   Put_Line("NB: If you are under Windows, please toggle NOW to full-screen");
   Put_Line("    mode (Alt-Return) if this text appears in a window.");
   New_Line;
   Put_Line("Which demo would you like to run ?" );
   New_Line;
   Put_Line("   Demo_3D_00:   rotating objects .................... 0" );
   Put_Line("   Demo_3D_01:   portal demo - pseudo 3D game ........ 1" );
   New_Line;
   Put_Line("   Quit .............................................. 9" );
   New_Line;
   ok:= True;
   begin
     Put("Choice: "); Get( n ); Skip_Line;
     case n is
       when      0 => Demo_3D_00;
       when      1 => Demo_3D_01;
       when      9 => exit;
       when others => Put_Line( "Wrong choice!" ); ok:= False;
     end case;
   exception
     when Data_error =>
       Skip_Line; Put_Line("Valid number, please!"); ok:= False;
   end;
   -- exit when ok;
  end loop;

end Demos;
