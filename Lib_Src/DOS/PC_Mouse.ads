------------------------------------------------------------------------------
--  File:            PC_Mouse.ads      (possibly extracted from DOS_PAQS.ZIP)
--  Description:     Simple mouse interface for PC  (GNAT/DOS Ada compiler)
--  Date/version:    12.IV.1999 / 28.III.1999 / 29.3.1997
--  Author:          Gautier de Montmollin
--  Uses:            DJGPP_Library (V0.5+ by Jerry van Dijk)
--  Documentation:   Robert B. Sutton
------------------------------------------------------------------------------

package PC_Mouse is

  procedure MouseReset;                               -- Always begin with it!
  MouseInstalled: Boolean;                            -- (Re)set by MouseReset

  procedure ShowMouse;
  procedure HideMouse;
  procedure MouseOn  renames ShowMouse;
  procedure MouseOff renames HideMouse;
  procedure SetSpeed(hr, vr : Integer);                   -- Set speed factors
  procedure SetAccel(threshold: Integer);                 -- Set accel. factor
  procedure SetArea(Left, Up, Right, Down : Integer);     -- Set area
  procedure SetPosition(hPos, vPos : Integer);            -- Set position
  procedure Mouse(hPos, vPos, Status: out Integer);       -- Read x,y,status
  procedure MouseXY(hPos, vPos: out Integer);             -- Read x,y
  function  MouseStatus return Integer;                   -- Read status

  ButtonL: constant Integer := 0;
  ButtonR: constant Integer := 1;
  ButtonM: constant Integer := 2;

  function  MouseClick(Button: Integer) return Integer;   -- # Clicks
  procedure MouseLRClick(l,r: out Integer);               --   ", left & right

  -- (1999) Clean methods to reflect button status:

  type Mouse_button is (left, right, middle);
  type Mouse_button_bunch is array( Mouse_button ) of Boolean;

  function Is_button_pressed(b: Mouse_button) return Boolean;
  -- all buttons:
  procedure Which_buttons_pressed( btns: out Mouse_button_bunch );
  -- all at once:
  procedure Mouse(hPos, vPos: out Integer; btns: out Mouse_button_bunch);

  -- Text mode mouse instructions:

  procedure SetTextArea(Left, Up, Right, Down: Integer);
  procedure TextMouse(hPos, vPos, Status: out Integer);  -- use for text mode
  procedure TextMouseXY(hPos, vPos: out Integer);        -- idem

  -- Graphic mode mouse instructions:

  type  Std_GCursor is (arrow, inverted_arrow,
    check, cross, finger, i_beam,
    left, right, up, down,
    horizontal,vertical, diagonal_1,diagonal_2);

  type  G_mouse_bitmap is array(0..15) of Integer;
  type  GCursor is record
    ScreenMask, CursorMask : G_mouse_bitmap;
    hotX, hotY : Integer;
  end record;

  procedure G_Initialize(x1,y1,x2,y2:Integer);
  procedure G_StdCursor(cursor: Std_GCursor);
  procedure G_SetCursor(cursor: GCursor);
  procedure G_ConditionalHide(left, top, right, bottom: Integer);

end PC_Mouse;
