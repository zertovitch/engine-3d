-----------------------------------------------------------------------------
--  File: gamedriv.adb; see specification (gamedriv.ads)
-----------------------------------------------------------------------------

package body Game_driving is

  procedure Record_commands(
              c          : in out command_set; -- commands are _added_ to c
              centre_x,
              centre_y,
              size_x,
              size_y     : in  Integer; -- screen dimensions for mouse
              gx,gy      : out Float    -- mouse movement since last call
            ) is

    use Multi_Keys, PC_Mouse;

    btns: Mouse_button_bunch;
    mx,my: Integer;
    glisse: Boolean;
    sensib: constant:= 8.0;

    begin
     -- Clavier: lettres: cl. americain
     c(turn_up):=        keyboard( key_pgup );
     c(turn_down):=      keyboard( key_pgdn );
     c(slide_up):=       keyboard( 30 ); -- A
     c(slide_down):=     keyboard( 44 ); -- Z
     c(swing_plus):=     keyboard( 32 );
     c(swing_minus):=    keyboard( 31 );
     c(photo):=          Strike_1( 45 );
     c(interrupt_game):= keyboard( key_esc );
     c(go_forward):=     keyboard( key_up );
     c(go_backwards):=   keyboard( key_down );
     c(run_mode):=       keyboard( key_lshft ) or keyboard( key_rshft );

     glisse:=  keyboard( key_alt );

     -- Souris:
     if MouseInstalled then
      Mouse( mx,my, btns );
      if btns( left )  then c(go_forward):= True; end if;
      if btns( right ) then glisse:= True; end if;
      -- contr“le par mouvement de la souris
      mx:= mx - centre_x;
      my:= my - centre_y;
      if mx/=0 or else my/=0 then
        if mx/=0 then
         gx:= sensib * Float(mx)/Float(size_x);
         if glisse then
           c(slide_lateral_graduated):= True;
         else
           c(turn_lateral_graduated) := True;
         end if;
        end if;
        if my/=0 then
         gy:= -sensib * Float(my)/Float(size_y);
         c(go_graduated):= True;
        end if;
        SetPosition( centre_x, centre_y ); -- remettre au centre la souris
      end if;

     end if;

     -- 26-Mar-2001: touches glisse ou tourne
     if glisse then
       c(slide_left):=     keyboard( key_left );
       c(slide_right):=    keyboard( key_right );
     else
       c(turn_left):=      keyboard( key_left );
       c(turn_right):=     keyboard( key_right );
     end if;

    end Record_commands;

end Game_driving;