-----------------------------------------------------------------------------
--  File: pcmouse.adb; see specification (pcmouse.ads)
--
--  Mouse procedures use interrupt 33H
--
--  Page references are to book: (SAMS) Assembly Language,
--                                      For Real Programmers Only
--
--  V0.1    12/12/99    RBS     Added boxed remarks and information
--
-----------------------------------------------------------------------------
with Interfaces, Interfaces.C, DJGPP_Library;
use  Interfaces, Interfaces.C, DJGPP_Library;

package body PC_Mouse is

  regs: dpmi_regs;

  procedure MI(A,B,C,D,E,SI,DI: Integer:= 0) is  -- mouse interrupt
  begin
    regs.ax := unsigned_16(A);
    regs.bx := unsigned_16(B);
    regs.cx := unsigned_16(C);
    regs.dx := unsigned_16(D);
    regs.es := unsigned_16(E);
    regs.si := unsigned_16(SI);
    regs.di := unsigned_16(DI);
    dpmi_int(16#33#, regs);
  end;

  -----------------------------------------------------------------------------
  --
  --  MouseReset:     Initializes the mouse driver software and rests
  --                  the mouse hardware.
  --                  Returns Boolean:  TRUE  = Mouse installed
  --                                    FALSE = Mouse not installed
  --
  --  Int 33H, Func 00H  Initialize Mouse Driver      (Page 1163)
  ----------------------------------------------------------------------------
  procedure MouseReset is begin MI(0); MouseInstalled:= (regs.AX/=0); end;

  ----------------------------------------------------------------------------
  --
  --  ShowMouse:      Causes the mouse cursor to appear on the display.
  --
  --  MouseOn:        A rename of Showmouse.
  --
  --  Int 33H, Func 01H  Show Mouse Cursor            (Page 1163)
  ----------------------------------------------------------------------------
  procedure ShowMouse is  begin MI(1); end;

  ----------------------------------------------------------------------------
  --
  --  HideMouse:      Causes the mouse cursor to disappear from the display.
  --
  --  MouseOff:       A rename of Hidemouse.
  --
  --  Int 33H, Func 02H  Hide Mouse Cursor            (Page 1163)
  ----------------------------------------------------------------------------
  procedure HideMouse is  begin MI(2); end;

  ----------------------------------------------------------------------------
  --
  --  Mouse:          Calls MouseXY
  --
  --  MouseXY:        Gets the current mouse coordinates and button status
  --                  Return X, Y
  --
  --  MouseStatus:    Returns the current button status
  --
  --  Int 33H, Func 03H  Get Mouse Coordinates        (Page 1163 / 1164)
  ----------------------------------------------------------------------------
  procedure Mouse(hPos, vPos, Status : out Integer) is
  begin MouseXY(hPos,vPos); Status:= Integer(regs.BX); end;

  procedure MouseXY(hPos, vPos: out Integer) is
  begin MI(3); hPos:= Integer(regs.CX);  vPos:= Integer(regs.DX); end;

  function MouseStatus return Integer is
  begin MI(3); return Integer(regs.BX); end;

  ----------------------------------------------------------------------------
  --
  --  MouseClick:     Gets information about the specified button.  Where,
  --                  and how often pressed
  --
  --  MouseLRClick:   Call MouseClick
  --
  --  Int 33H, Func 05H  Get Button Press Status      (Page 1164)
  ----------------------------------------------------------------------------
  function  MouseClick(Button: Integer) return Integer is
  begin  MI(5, Button); return Integer(regs.BX); end;

  procedure MouseLRClick(l,r: out Integer) is
  begin  l:=MouseClick(ButtonL); r:=MouseClick(ButtonR); end;

  ----------------------------------------------------------------------------
  --
  --  SetPosition:    Sets new mouse coordinates
  --
  --  Int 33h, Func 04H  Set Mouse Coordinates.       (Page 1164)
  ----------------------------------------------------------------------------
  procedure SetPosition(hPos, vPos: Integer) is begin MI(4, 0,hPos,vPos); end;

  ----------------------------------------------------------------------------
  --
  --  SetSpeed:   Sets ratio:  mouse motion (mickeys) : coord. changes (pixels)
  --              This is an inverse function. The higher the ratio, the slower
  --              the mouse moves.
  --
  --  Int 33h, Func 0FH  Set Mouse Speed.             (Page 1169)
  ----------------------------------------------------------------------------
  procedure SetSpeed(hr, vr : Integer) is       begin MI(15, 0,hr,vr); end;

  ----------------------------------------------------------------------------
  --
  --  SetAccel:       Sets the speed, in mickeys per second, at which the
  --                  cursor will double in speed.
  --
  --  Int 33H, Func 13H  Set Double Speed Threshold.  (Page 1170)
  ----------------------------------------------------------------------------
  procedure SetAccel(threshold: Integer) is  begin MI(19, 0,0,threshold); end;

  ----------------------------------------------------------------------------
  --
  --  SetArea:    Sets the left, right, top and bottom limits for the mouse,
  --              defining an area within which it may be active.
  --              This is a call to two functions, o7H and 08H.
  --
  --  Int 33H, Func 07H  Set Mouse Horizontal Limits. (Page 1165)
  --  Int 33H, Func 08H  Set Mouse Vertical Limits.   (Page 1166)
  ----------------------------------------------------------------------------
  procedure SetArea(Left, Up, Right, Down: Integer) is
  begin  MI(7 ,0, Left,Right); MI(8 ,0, Up,  Down); end;


  ---------------------------------------------------------------------------
  --
  --  Added 'Clean' methods to reflect mouse status
  --
  --  GDM - 1999
  ---------------------------------------------------------------------------
  function Is_button_pressed(b: Mouse_button) return Boolean is
    s: constant Integer:= MouseStatus;
  begin
    return (s / (2**Mouse_button'Pos(b))) mod 2 = 1;
  end;

  procedure Which_buttons_pressed( btns: out Mouse_button_bunch ) is
    s: constant Integer:= MouseStatus;
  begin
    for b in Mouse_button loop
      btns(b):= (s / (2**Mouse_button'Pos(b))) mod 2 = 1;
    end loop;
  end;

  procedure Mouse(hPos, vPos: out Integer; btns: out Mouse_button_bunch) is
  begin
    Which_buttons_pressed( btns );
    hPos:= Integer(regs.CX);  vPos:= Integer(regs.DX);
  end;

  ---------------------------------------------------------------------------
  --
  --  Procedures for Text use of Mouse
  --
  --
  ---------------------------------------------------------------------------
  procedure SetTextArea(Left, Up, Right, Down: Integer) is
  begin  SetArea( left*8-8, up*8-8, right*8-8, down*8-8 ); end;

  procedure TextMouse(hPos, vPos, Status: out Integer) is
  begin Mouse(hPos, vPos, Status); hpos:= hpos / 8; vpos:= vpos / 8; end;

  procedure TextMouseXY(hPos, vPos: out Integer) is
    dummy: Integer; begin TextMouse(hPos,vPos,dummy); end;

  ---------------------------------------------------------------------------
  --
  --  Procedures for Graphics use of Mouse
  --
  --
  ---------------------------------------------------------------------------
  procedure G_Initialize(x1,y1,x2,y2:Integer) is
  begin
    SetArea( x1,y1,x2,y2 );
    G_StdCursor( arrow );
    SetPosition( (x2-x1) / 2, (y2-y1) / 2);
    MouseOn;
  end;

  procedure G_SetCursor(cursor: in GCursor) is
    tmp: char_array(0..63);

    function lo(w:unsigned_16) return unsigned_8 is
    begin return unsigned_8(w and 16#00FF#); end;

    function hi(w:unsigned_16) return unsigned_8 is
    begin return unsigned_8(shift_right(w,8) and 16#00FF#); end;

    function byte2chr(b:unsigned_8) return char is
    begin return To_C(Character'Val(Integer(b))); end;

  begin
    if current_info.size_of_transfer_buffer < tmp'Length then
      raise Storage_Error;
    end if;
    for i in G_mouse_bitmap'Range loop
      tmp(size_t(i*2)):=    byte2chr(lo(unsigned_16(cursor.screenmask(i))));
      tmp(size_t(i*2+1)):=  byte2chr(hi(unsigned_16(cursor.screenmask(i))));
      tmp(size_t(i*2+32)):= byte2chr(lo(unsigned_16(cursor.cursormask(i))));
      tmp(size_t(i*2+33)):= byte2chr(hi(unsigned_16(cursor.cursormask(i))));
    end loop;
    dosmemput(
      Offset => current_info.linear_address_of_transfer_buffer,
      Length => tmp'Length,
      Buffer => tmp);

    -- TP: WITH cursor DO MI(9,hotX,hotY, Ofs(ScreenMask), Seg(ScreenMask))
    MI(9,cursor.hotX,cursor.hotY,
      Integer(
        current_info.linear_address_of_transfer_buffer and 16#0F#)
      ,
      Integer(
        Shift_Right(current_info.linear_address_of_transfer_buffer,
          4) and 16#FFFF#)
      );
  end;                          -- Ouf...

  procedure G_StdCursor(cursor: Std_GCursor ) is

    Std: constant array(Std_GCursor) of GCursor :=

      (arrow =>
      (  ScreenMask => (16#1FFF#, 16#0FFF#, 16#07FF#, 16#03FF#,
          16#01FF#, 16#00FF#, 16#007F#, 16#003F#,
          16#001F#, 16#003F#, 16#01FF#, 16#01FF#,
          16#E0FF#, 16#F0FF#, 16#F8FF#, 16#F8FF# ),
        CursorMask => (16#0000#, 16#4000#, 16#6000#, 16#7000#,
          16#7800#, 16#7C00#, 16#7E00#, 16#7F00#,
          16#7F80#, 16#7C00#, 16#4C00#, 16#0600#,
          16#0600#, 16#0300#, 16#0300#, 16#0000# ),
        hotX => 16#0001#,  hotY => 16#0001# ) ,

      inverted_arrow => (
        ScreenMask => (
          16#FFF8# ,16#FFF0# ,16#FFE0# ,16#FFC0# ,
          16#FF80# ,16#FF00# ,16#FE00# ,16#FC00# ,
          16#FC00# ,16#FC00# ,16#FF00# ,16#FF00# ,
          16#FE08# ,16#FE0F# ,16#FE1F# ,16#FE1F#  ),
        CursorMask => (
          16#0000# ,16#0002# ,16#0006# ,16#000E# ,
          16#001E# ,16#003E# ,16#007E# ,16#00FE# ,
          16#01FE# ,16#003E# ,16#0036# ,16#0062# ,
          16#0060# ,16#00C0# ,16#00C0# ,16#0000#  ),
        hotX => 14, hotY => 1),

      check =>
      (    ScreenMask => (16#FFF0#, 16#FFE0#, 16#FFC0#, 16#FF81#,
          16#FF03#, 16#0607#, 16#000F#, 16#001F#,
          16#803F#, 16#C07F#, 16#E0FF#, 16#F1FF#,
          16#FFFF#, 16#FFFF#, 16#FFFF#, 16#FFFF# ),
        CursorMask => (16#0000#, 16#0006#, 16#000C#, 16#0018#,
          16#0030#, 16#0060#, 16#70C0#, 16#3980#,
          16#1F00#, 16#0E00#, 16#0400#, 16#0000#,
          16#0000#, 16#0000#, 16#0000#, 16#0000# ),
        hotX => 16#0005#,  hotY => 16#0010# ),

      cross => (
        ScreenMask => (
          16#FE3F# ,16#FE3F# ,16#FE3F# ,16#FE3F# ,
          16#FE3F# ,16#FE3F# ,16#8000# ,16#8080# ,
          16#8000# ,16#FE3F# ,16#FE3F# ,16#FE3F# ,
          16#FE3F# ,16#FE3F# ,16#FE3F# ,16#FFFF#  ),
        CursorMask => (
          16#0000# ,16#0080# ,16#0080# ,16#0080# ,
          16#0080# ,16#0080# ,16#0000# ,16#3E3E# ,
          16#0000# ,16#0080# ,16#0080# ,16#0080# ,
          16#0080# ,16#0080# ,16#0000# ,16#0000#  ),
        hotX => 8, hotY => 7),

      finger =>
      (  ScreenMask => (16#F3FF#, 16#E1FF#, 16#E1FF#, 16#E1FF#,
          16#E1FF#, 16#E049#, 16#E000#, 16#8000#,
          16#0000#, 16#0000#, 16#07FC#, 16#07F8#,
          16#9FF9#, 16#8FF1#, 16#C003#, 16#E007# ),
        CursorMask => (16#0C00#, 16#1200#, 16#1200#, 16#1200#,
          16#1200#, 16#13B6#, 16#1249#, 16#7249#,
          16#9249#, 16#9001#, 16#9001#, 16#8001#,
          16#4002#, 16#4002#, 16#2004#, 16#1FF8# ),
        hotX => 16#0004#,  hotY => 16#0000# ),

      i_beam => (
        ScreenMask => (
          16#F80F# ,16#F80F# ,16#F80F# ,16#FE3F# ,
          16#FE3F# ,16#FE3F# ,16#FE3F# ,16#FE3F# ,
          16#FE3F# ,16#FE3F# ,16#FE3F# ,16#FE3F# ,
          16#FE3F# ,16#F80F# ,16#F80F# ,16#F80F#  ),
        CursorMask => (
          16#0000# ,16#0360# ,16#0080# ,16#0080# ,
          16#0080# ,16#0080# ,16#0080# ,16#0080# ,
          16#0080# ,16#0080# ,16#0080# ,16#0080# ,
          16#0080# ,16#0080# ,16#0360# ,16#0000#  ),
        hotX => 8, hotY => 7),

      left => (
        ScreenMask => (
          16#FFC7# ,16#FF87# ,16#FF07# ,16#FE07# ,
          16#FC07# ,16#F807# ,16#F007# ,16#E007# ,
          16#F007# ,16#F807# ,16#FC07# ,16#FE07# ,
          16#FF07# ,16#FF87# ,16#FFC7# ,16#FFFF#  ),
        CursorMask => (
          16#0000# ,16#0010# ,16#0030# ,16#0070# ,
          16#00F0# ,16#01F0# ,16#03F0# ,16#07F0# ,
          16#03F0# ,16#01F0# ,16#00F0# ,16#0070# ,
          16#0030# ,16#0010# ,16#0000# ,16#0000#  ),
        hotX => 5, hotY => 7),

      right => (
        ScreenMask => (
          16#F1FF# ,16#F0FF# ,16#F07F# ,16#F03F# ,
          16#F01F# ,16#F00F# ,16#F007# ,16#F003# ,
          16#F007# ,16#F00F# ,16#F01F# ,16#F03F# ,
          16#F07F# ,16#F0FF# ,16#F1FF# ,16#FFFF#  ),
        CursorMask => (
          16#0000# ,16#0400# ,16#0600# ,16#0700# ,
          16#0780# ,16#07C0# ,16#07E0# ,16#07F0# ,
          16#07E0# ,16#07C0# ,16#0780# ,16#0700# ,
          16#0600# ,16#0400# ,16#0000# ,16#0000#  ),
        hotX => 11, hotY => 7),

      up => (
        ScreenMask => (
          16#FFFF# ,16#FFFF# ,16#FEFF# ,16#FC7F# ,
          16#F83F# ,16#F01F# ,16#E00F# ,16#C007# ,
          16#8003# ,16#0001# ,16#0001# ,16#0001# ,
          16#FFFF# ,16#FFFF# ,16#FFFF# ,16#FFFF#  ),
        CursorMask => (
          16#0000# ,16#0000# ,16#0000# ,16#0000# ,
          16#0100# ,16#0380# ,16#07C0# ,16#0FE0# ,
          16#1FF0# ,16#3FF8# ,16#7FFC# ,16#0000# ,
          16#0000# ,16#0000# ,16#0000# ,16#0000#  ),
        hotX => 7, hotY => 4),

      down => (
        ScreenMask => (
          16#FFFF# ,16#FFFF# ,16#FFFF# ,16#0001# ,
          16#0001# ,16#0001# ,16#8003# ,16#C007# ,
          16#E00F# ,16#F01F# ,16#F83F# ,16#FC7F# ,
          16#FEFF# ,16#FFFF# ,16#FFFF# ,16#FFFF#  ),
        CursorMask => (
          16#0000# ,16#0000# ,16#0000# ,16#0000# ,
          16#7FFC# ,16#3FF8# ,16#1FF0# ,16#0FE0# ,
          16#07C0# ,16#0380# ,16#0100# ,16#0000# ,
          16#0000# ,16#0000# ,16#0000# ,16#0000#  ),
        hotX => 7, hotY => 10),

      horizontal => (
        ScreenMask => (
          16#FFFF# ,16#FFFF# ,16#FFFF# ,16#FFFF# ,
          16#FFFF# ,16#C7E3# ,16#87E1# ,16#0000# ,
          16#0000# ,16#0000# ,16#87E1# ,16#C7E3# ,
          16#FFFF# ,16#FFFF# ,16#FFFF# ,16#FFFF#  ),
        CursorMask => (
          16#0000# ,16#0000# ,16#0000# ,16#0000# ,
          16#0000# ,16#0000# ,16#1008# ,16#2004# ,
          16#7FFE# ,16#2004# ,16#1008# ,16#0000# ,
          16#0000# ,16#0000# ,16#0000# ,16#0000#  ),
        hotX => 8, hotY => 8),

      vertical => (
        ScreenMask => (
          16#FC7F# ,16#F83F# ,16#F01F# ,16#F01F# ,
          16#F01F# ,16#FC7F# ,16#FC7F# ,16#FC7F# ,
          16#FC7F# ,16#FC7F# ,16#FC7F# ,16#F01F# ,
          16#F01F# ,16#F01F# ,16#F83F# ,16#FC7F#  ),
        CursorMask => (
          16#0000# ,16#0100# ,16#0380# ,16#0540# ,
          16#0100# ,16#0100# ,16#0100# ,16#0100# ,
          16#0100# ,16#0100# ,16#0100# ,16#0100# ,
          16#0540# ,16#0380# ,16#0100# ,16#0000#  ),
        hotX => 7, hotY => 8),

      diagonal_1 => (
        ScreenMask => (
          16#01FF# ,16#01FF# ,16#01FF# ,16#03FF# ,
          16#01FF# ,16#00FF# ,16#107F# ,16#F83F# ,
          16#FC1F# ,16#FE08# ,16#FF00# ,16#FF80# ,
          16#FFC0# ,16#FF80# ,16#FF80# ,16#FF80#  ),
        CursorMask => (
          16#0000# ,16#7C00# ,16#6000# ,16#5000# ,
          16#4800# ,16#4400# ,16#0200# ,16#0100# ,
          16#0080# ,16#0040# ,16#0022# ,16#0012# ,
          16#000A# ,16#0006# ,16#003E# ,16#0000#  ),
        hotX => 8, hotY => 8),

      diagonal_2 => (
        ScreenMask => (
          16#FF80# ,16#FF80# ,16#FF80# ,16#FFC0# ,
          16#FF80# ,16#FF00# ,16#FE08# ,16#FC1F# ,
          16#F83F# ,16#107F# ,16#00FF# ,16#01FF# ,
          16#03FF# ,16#01FF# ,16#01FF# ,16#01FF#  ),
        CursorMask => (
          16#0000# ,16#003E# ,16#0006# ,16#000A# ,
          16#0012# ,16#0022# ,16#0040# ,16#0080# ,
          16#0100# ,16#0200# ,16#4400# ,16#4800# ,
          16#5000# ,16#6000# ,16#7C00# ,16#0000#  ),
        hotX => 8, hotY => 7)
      );

  begin
    G_SetCursor(std(cursor));
  end;

  procedure G_ConditionalHide( left, top, right, bottom: Integer ) is
  begin  MI(10, 0, left,top,0,right,bottom); end;

end PC_Mouse;
