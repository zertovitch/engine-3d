------------------------------------------------------------------------------
--  File:            Multkeys.ads       (possibly extracted from DOS_PAQS.ZIP)
--  Description:     Keyboard handler for scanning multiple keys
--                      For a test, see tests\temulkey.adb in same archive
--  Date/version:    4-Aug-2004 / 11-Dec-2000 / 4.VIII.1999
--
--  Copyright (c) Gautier de Montmollin 1999..2004
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

-- Handler tested on:
--   MS-DOS  7.1    : OK (Win 9x MS-DOS mode)
--   DR-DOS  7.03   : OK with DPMI ON mode, and both Single & Multi tasking
--   MS Windows 4.0 : OK (Win 9x GUI mode)
--   MS Windows NT  : OK (NT 4, build 1381)

-- Abbreviations:
--   MS: Microsoft format (MS-DOS & MS Windows)
--   DR: Digital Research format (Novell-Caldera-Lineo DR-DOS)

package Multi_keys is

  pragma Elaborate_Body;

  -- *** Keyboard map

  subtype Key_value is Natural range 0..127;

  -- keyboard(i) <=> the key number i is pressed

  keyboard: array(Key_value) of Boolean:= (others=> False);

  -- some common values

  key_esc  : constant:= 01;
  key_space: constant:= 57;

  key_left : constant:= 75;
  key_right: constant:= 77;
  key_up   : constant:= 72;
  key_down : constant:= 80;
  key_pgup : constant:= 73;
  key_pgdn : constant:= 81;
  key_home : constant:= 71;
  key_end  : constant:= 79;

  key_gray_plus   : constant:= 78;
  key_gray_minus  : constant:= 74;
  key_gray_star   : constant:= 55;
  key_sys_req     : constant:= 84;
  key_scroll_lock : constant:= 70;

  key_F1   : constant:= 59;
  key_F2   : constant:= 60;
  key_F3   : constant:= 61;
  key_F4   : constant:= 62;
  key_F5   : constant:= 63;
  key_F6   : constant:= 64;
  key_F7   : constant:= 65;
  key_F8   : constant:= 66;
  key_F9   : constant:= 67;
  key_F10  : constant:= 68;
  key_F11  : constant:= 87;
  key_F12  : constant:= 88;

  key_ins  : constant:= 82;
  key_del  : constant:= 83;
  key_ctrl : constant:= 29;
  key_alt  : constant:= 56;
  key_lshft: constant:= 42;
  key_rshft: constant:= 54;
  key_caps : constant:= 58;

  procedure Install;
  procedure Uninstall;

  function Pressed_keys return Natural;

  function Is_a_key_pressed return Boolean;

  -- *** National keyboards (as in DR-DOS Keyb API)

  type kb_country is
    (US, FR, GR, UK, DK, SV, SU, IT, SP, NO, PO,
     BE, NL, CF, LA, SF, SG, RU, TQ, TF, HU, BR);

  -- Codes: full output of KEYB /? in DR-DOS 7.03 (1999)

  -- KEYB 2.12    National keyboard utility
  -- Copyright (c) 1988,1997 Caldera, Inc.  All rights reserved.
  --
  -- KEYB [/H] code[+|-][,codepage][/MH][/MU][/ML]
  --  Country               Codepages         Country               Codepages
  -- BE - Belgium           437   850        PO - Portugal          860   850
  -- BR - Brazil            437   850        RU - Russia            866   850
  -- CF - Canada (French)   863   850        SF - Swiss (French)    437   850
  -- DK - Denmark           865   850        SG - Swiss (German)    437   850
  -- FR - France            437   850        SP - Spain             437   850
  -- GR - Germany           437   850        SV - Sweden            437   850
  -- HU - Hungary           852   850        SU - Finland           437   850
  -- IT - Italy             437   850        TF - Turkey (FGGIOD)   857   850
  -- LA - Latin America     437   850        TQ - Turkey (QWERTY)   857   850
  -- NL - Netherlands       437   850        UK - United Kingdom    437   850
  -- NO - Norway            865   850        US - United States     437   850
  -- +     Force keyboard type to enhanced
  -- -     Force keyboard type to non-enhanced
  -- /MH   Load KEYB into high (HMA) memory
  -- /MU   Load KEYB into upper memory
  -- /ML   Load KEYB into conventional memory
  -- If codepage is omitted then the 1st, (local),
  -- codepage shown will be assumed.

  function Detect_country return kb_country; -- through MS/DR Keyb API

  -- You can override the detected country value.
  -- NB: doesn't change any DOS setting.

  country: kb_country;

  -- Converts scan code to country-dependant character (NUL for wrong keys)
  -- in 850 code page encoding

  function Key_ASCII ( scancode: Key_value; upper: Boolean ) return Character;

  -- Returns a rich, country-sensitive, image of a key
  --
  -- e.g. "Ctrl", "Alt",... "q", "Q",... "ESC", "CR",... ""

  function Key_image ( scancode: Key_value; upper: Boolean ) return String;

  -- Returns true only *once* per key pression
  function Strike_1 ( scancode: Key_value ) return Boolean;

end Multi_keys;
