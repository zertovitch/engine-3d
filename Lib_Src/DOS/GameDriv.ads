------------------------------------------------------------------------------
--  File:            Gamedriv.ads
--  Description:     Command set for games
--  Date / Version:  20-Jul-2001 ; 26-Mar-2001 ; 25-Dec-1999
--  Author:          Gautier de Montmollin
--
--  Copyright (c) Gautier de Montmollin 1999 .. 2001
------------------------------------------------------------------------------
--  Latest changes:
--   26-Mar-2001: "translate_" becomes "slide_" and is programmed

--  To do: programmable behaviour

with Multi_keys, PC_mouse;

package Game_driving is

  type Command is (
    go_forward, go_backwards, go_graduated,
    slide_left, slide_right, slide_lateral_graduated,
    turn_left, turn_right, turn_lateral_graduated,
    slide_up, slide_down, slide_vertical_graduated,
    turn_up, turn_down, turn_vertical_graduated,
    run_mode,
    swing_plus, swing_minus,
    photo, interrupt_game, bogus_command );

  type Command_set is array( Command ) of Boolean;

  -- The empty command set:
  no_command: constant Command_set:= (others=> False);

  keyboard_command_mapping: array( Multi_keys.key_value ) of Command :=
    ( others=> bogus_command ); -- for later !!

  mouse_command_mapping   : array( PC_Mouse.Mouse_button ) of Command :=
    ( others=> bogus_command ); -- for later !!

  -- Record game commands from peripherals (keyboard, mouse) --
  procedure Record_commands(
              c          : in out Command_set; -- commands are _added_ to c
              centre_x,
              centre_y,
              size_x,
              size_y     : in  Integer; -- screen dimensions for mouse
              gx,gy      : out Float    -- mouse movement since last call
            );

end Game_driving;