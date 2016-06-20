--  11-Feb-2002: GdM : - Added Use_own_font for Graphics_Mode
--  24-Jul-2001: GdM : - Added Force_banked for Graphics_Mode under NT
--                     - Added Get_available_modes
-----------------------------------------------------------------------
--
--  File:        svga.ads
--  Description: 256 color SVGA driver for VBE version 1.2 and higher
--  Rev:         0.3
--  Date:        6-Nov-2000
--  Author:      Jerry van Dijk
--  Mail:        jdijk@acm.org
--
--  Copyright (c) Jerry van Dijk, 1996-2000
--  Billie Holidaystraat 28
--  2324 LK Leiden
--  THE NETHERLANDS
--  tel int +31 (0)71 531 4365
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation, Ada.Finalization;

package SVGA is

   ---------------------------------------
   -- Specify the supported video modes --
   ---------------------------------------

   type SVGA_Mode is (M640x400, M640x480, M800x600,
                      M1024x768, M1280x1024, M1600x1200,
                      Autodetect);
   for SVGA_Mode use (M640x400 => 16#0100#, M640x480 => 16#0101#,
                      M800x600 => 16#0103#, M1024x768 => 16#0105#,
                      M1280x1024 => 16#0107#, M1600x1200 => 16#0120#,
                      Autodetect => 16#FFFF#);
   for SVGA_Mode'Size use 16;

   subtype Video_Mode is SVGA_Mode range M640x400 .. M1600x1200;
   type Video_Mode_Set is array (Video_Mode) of Boolean;

   ---------------------------
   -- Specify color palette --
   ---------------------------

   Num_Colors : constant := 256;

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;

   subtype Color_Type is Byte;
   subtype Color_Value is Byte range 0 .. 63;

   type RGB_Color is
      record
         Red   : Color_Value;
         Green : Color_Value;
         Blue  : Color_Value;
      end record;

   type Color_Palette is array (Color_Type) of RGB_Color;

   --  The standard colors below are only valid for the default palette!

   Standard_Black         : constant Color_Type := 0;
   Standard_Blue          : constant Color_Type := 1;
   Standard_Green         : constant Color_Type := 2;
   Standard_Cyan          : constant Color_Type := 3;
   Standard_Red           : constant Color_Type := 4;
   Standard_Magenta       : constant Color_Type := 5;
   Standard_Brown         : constant Color_Type := 6;
   Standard_Light_Gray    : constant Color_Type := 7;
   Standard_Dark_Gray     : constant Color_Type := 8;
   Standard_Light_Blue    : constant Color_Type := 9;
   Standard_Light_Green   : constant Color_Type := 10;
   Standard_Light_Cyan    : constant Color_Type := 11;
   Standard_Light_Red     : constant Color_Type := 12;
   Standard_Light_Magenta : constant Color_Type := 13;
   Standard_Yellow        : constant Color_Type := 14;
   Standard_White         : constant Color_Type := 15;


   --------------------------
   -- Specify display size --
   --------------------------

   subtype Y_Loc is Natural range 0 .. 1199;
   subtype X_Loc is Natural range 0 .. 1599;


   ---------------------------------------
   -- Specify actual display parameters --
   ---------------------------------------

   X_Max : X_Loc;
   Y_Max : Y_Loc;
   X_Size, Y_Size : Natural;


   ------------------------------
   -- Specify system font size --
   ------------------------------

   Font_Width  : constant Positive :=  8;
   Font_Height : constant Positive := 14;


   -----------------------------
   -- Specify a screen buffer --
   -----------------------------

   type Screen_Buffer (New_Width, New_Height : Natural) is limited private;


   ---------------------
   -- Control display --
   ---------------------

   function Get_available_modes return Video_Mode_Set;

   procedure Graphics_Mode (Mode         : in SVGA_Mode := Autodetect;
                            Force_banked : in Boolean:= False;
                            Use_own_font : in Boolean:= False);

   function In_Graphics_Mode return Boolean;

   function Linear_Frame_Buffer return Boolean;

   procedure Text_Mode;

   procedure Wait_For_Vertical_Retrace;


   ---------------------
   -- Get/Set palette --
   ---------------------

   procedure Restore_Default_Palette;

   function Get_Palette return Color_Palette;

   procedure Set_Palette (Palette : in Color_Palette);

   function Get_Color (Color : Color_Type) return RGB_Color;

   procedure Set_Color (Color : in Color_Type; Value : in RGB_Color);


   --------------------
   -- Get/Set pixels --
   --------------------

   function Get_Pixel (X : in X_Loc;
                       Y : in Y_Loc) return Color_Type;

   procedure Set_Pixel (X     : in X_Loc;
                        Y     : in Y_Loc;
                        Color : in Color_Type := Standard_White);

   function Get_Pixel (Buffer : in Screen_Buffer;
                       X      : in X_Loc;
                       Y      : in Y_Loc) return Color_Type;

   procedure Set_Pixel (Buffer : in out Screen_Buffer;
                        X      : in     X_Loc;
                        Y      : in     Y_Loc;
                        Color  : in     Color_Type := Standard_White);


   ----------------
   -- Draw lines --
   ----------------

   procedure Hor_Line (X1, X2 : in X_Loc;
                       Y      : in Y_Loc;
                       Color  : in Color_Type := Standard_White);

   procedure Ver_Line (X      : in X_Loc;
                       Y1, Y2 : in Y_Loc;
                       Color  : in Color_Type := Standard_White);

   procedure Line (X1    : in X_Loc;
                   Y1    : in Y_Loc;
                   X2    : in X_Loc;
                   Y2    : in Y_Loc;
                   Color : in Color_Type := Standard_White);

   procedure Frame_Rect (X1    : in X_Loc;
                         Y1    : in Y_Loc;
                         X2    : in X_Loc;
                         Y2    : in Y_Loc;
                         Color : in Color_Type := Standard_White);

   procedure Hor_Line (Buffer :    out Screen_Buffer;
                       X1, X2 : in     X_Loc;
                       Y      : in     Y_Loc;
                       Color  : in     Color_Type := Standard_White);

   procedure Ver_Line (Buffer :    out Screen_Buffer;
                       X      : in     X_Loc;
                       Y1, Y2 : in     Y_Loc;
                       Color  : in     Color_Type := Standard_White);

   procedure Line (Buffer :    out Screen_Buffer;
                   X1     : in     X_Loc;
                   Y1     : in     Y_Loc;
                   X2     : in     X_Loc;
                   Y2     : in     Y_Loc;
                   Color  : in     Color_Type := Standard_White);

   procedure Frame_Rect (Buffer :    out Screen_Buffer;
                         X1     : in X_Loc;
                         Y1     : in Y_Loc;
                         X2     : in X_Loc;
                         Y2     : in Y_Loc;
                         Color : in Color_Type := Standard_White);

   ------------------
   -- Draw circles --
   ------------------

   procedure Circle (X      : in X_Loc;
                     Y      : in Y_Loc;
                     Radius : in Positive;
                     Color  : in Color_Type := Standard_White);

   procedure Math_Circle (X      : in X_Loc;
                          Y      : in Y_Loc;
                          Radius : in Positive;
                          Color  : in Color_Type := Standard_White);

   procedure Circle (Buffer :    out Screen_Buffer;
                     X      : in     X_Loc;
                     Y      : in     Y_Loc;
                     Radius : in     Positive;
                     Color  : in     Color_Type := Standard_White);

   procedure Math_Circle (Buffer :    out Screen_Buffer;
                          X      : in     X_Loc;
                          Y      : in     Y_Loc;
                          Radius : in     Positive;
                          Color  : in     Color_Type := Standard_White);


   ----------------------
   -- Manipulate areas --
   ----------------------

   procedure Clear_Screen (Color : in Color_Type := Standard_Black);

   procedure Fill_Rect (X1    : in X_Loc;
                        Y1    : in Y_Loc;
                        X2    : in X_Loc;
                        Y2    : in Y_Loc;
                        Color : in Color_Type := Standard_White);

   procedure Clear_Screen (Buffer : in out Screen_Buffer;
                           Color  : in     Color_Type := Standard_Black);

   procedure Fill_Rect (Buffer :    out Screen_Buffer;
                        X1     : in     X_Loc;
                        Y1     : in     Y_Loc;
                        X2     : in     X_Loc;
                        Y2     : in     Y_Loc;
                        Color  : in     Color_Type := Standard_White);


   ----------------
   -- Write text --
   ----------------

   procedure Write_String (X     : in X_Loc;
                           Y     : in Y_Loc;
                           S     : in String;
                           Color : in Color_Type := Standard_White);

   procedure Write_String (Buffer : in out Screen_Buffer;
                           X      : in     X_Loc;
                           Y      : in     Y_Loc;
                           S      : in     String;
                           Color  : in     Color_Type := Standard_White);


   ------------------------------
   -- Screen buffer operations --
   ------------------------------

   procedure Put_Buffer (Buffer : in Screen_Buffer);

   procedure Get_Buffer (Buffer : out Screen_Buffer);

   procedure Put_Buffer (X      : in X_Loc;
                         Y      : in Y_Loc;
                         Buffer : in Screen_Buffer);

   procedure Get_Buffer (X      : in     X_Loc;
                         Y      : in     Y_Loc;
                         Buffer : in out Screen_Buffer);

   procedure Put_Buffer (Source      : in     Screen_Buffer;
                         X           : in     X_Loc;
                         Y           : in     Y_Loc;
                         Destination : in out Screen_Buffer);

   procedure Put_Buffer (Source      : in     Screen_Buffer;
                         X           : in     X_Loc;
                         Y           : in     Y_Loc;
                         Destination : in out Screen_Buffer;
                         Transparency: in     Color_Type);

   procedure Copy_Buffer (Source      : in     Screen_Buffer;
                          Destination : in out Screen_Buffer);

   ---------------------------------
   -- Package specific exceptions --
   ---------------------------------

   Out_Of_Range                 : exception;
   No_VBE_Driver                : exception;
   No_Valid_Modes               : exception;
   Out_Of_Buffer_Range          : exception;
   Screen_Buffer_Error          : exception;
   Invalid_VBE_Version          : exception;
   Not_In_Graphics_Mode         : exception;
   Unsupported_Video_Mode       : exception;
   Cannot_Free_Local_Desc       : exception;
   Cannot_Alloc_Local_Desc      : exception;
   Cannot_Set_Base_Address      : exception;
   Already_In_Graphics_Mode     : exception;
   Cannot_Set_Segment_Limit     : exception;
   Cannot_Map_Physical_Memory   : exception;

private

   -----------------------
   -- The screen buffer --
   -----------------------

   type Data_Buffer is array (Natural range <>) of Color_Type;
   type Data_Buffer_Access is access all Data_Buffer;

   procedure Free is
     new Ada.Unchecked_Deallocation (Data_Buffer, Data_Buffer_Access);

   type Screen_Buffer (New_Width, New_Height : Natural) is
     new Ada.Finalization.Limited_Controlled with
      record
         Width  : Natural := New_Width;
         Height : Natural := New_Height;
         Data   : aliased Data_Buffer_Access;
      end record;

   procedure Finalize (SB : in out Screen_Buffer);
   procedure Initialize (SB : in out Screen_Buffer);


   -------------------------------
   -- Inline the SVGA functions --
   -------------------------------

   pragma Inline (Line);
   pragma Inline (Circle);
   pragma Inline (Hor_Line);
   pragma Inline (Ver_Line);
   pragma Inline (Text_Mode);
   pragma Inline (Get_Color);
   pragma Inline (Set_Color);
   pragma Inline (Get_Pixel);
   pragma Inline (Set_Pixel);
   pragma Inline (Fill_Rect);
   pragma Inline (Put_Buffer);
   pragma Inline (Get_Buffer);
   pragma Inline (Get_Palette);
   pragma Inline (Set_Palette);
   pragma Inline (Math_Circle);
   pragma Inline (Clear_Screen);
   pragma Inline (Write_String);
   pragma Inline (Graphics_Mode);
   pragma Inline (In_Graphics_Mode);
   pragma Inline (Restore_Default_Palette);
   pragma Inline (Wait_For_Vertical_Retrace);

end SVGA;
