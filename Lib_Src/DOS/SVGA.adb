-----------------------------------------------------------------------
--
--  File:        svga.adb
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

with Ada.Unchecked_Deallocation;

with System;                  use System;
with Interfaces;              use Interfaces;
with System.Machine_Code;     use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

package body SVGA is

   -----------------------------------------------
   -- Import the intrinsic shift_right function --
   -----------------------------------------------
   function Shift_Right (Value : unsigned_32;
                         Count : Natural) return unsigned_32;
   pragma Import (Intrinsic, Shift_Right);

   -------------------------
   -- Processor registers --
   -------------------------
   type Dpmi_Regs is
      record
         Di     : Unsigned_16;
         Di_Hi  : Unsigned_16;
         Si     : Unsigned_16;
         Si_Hi  : Unsigned_16;
         Bp     : Unsigned_16;
         Bp_Hi  : Unsigned_16;
         Res    : Unsigned_16;
         Res_Hi : Unsigned_16;
         Bx     : Unsigned_16;
         Bx_Hi  : Unsigned_16;
         Dx     : Unsigned_16;
         Dx_Hi  : Unsigned_16;
         Cx     : Unsigned_16;
         Cx_Hi  : Unsigned_16;
         Ax     : Unsigned_16;
         Ax_Hi  : Unsigned_16;
         Flags  : Unsigned_16;
         Es     : Unsigned_16;
         Ds     : Unsigned_16;
         Fs     : Unsigned_16;
         Gs     : Unsigned_16;
         Ip     : Unsigned_16;
         Cs     : Unsigned_16;
         Sp     : Unsigned_16;
         Ss     : Unsigned_16;
      end record;
   pragma Convention (C, Dpmi_Regs);

   ------------------------------
   -- DOS extender information --
   ------------------------------
   type Go32_Info_Block is
      record
         Size_Of_This_Structure_In_Bytes       : Unsigned_32;
         Linear_Address_Of_Primary_Screen      : Unsigned_32;
         Linear_Address_Of_Secondary_Screen    : Unsigned_32;
         Linear_Address_Of_Transfer_Buffer     : Unsigned_32;
         Size_Of_Transfer_Buffer               : Unsigned_32;
         Pid                                   : Unsigned_32;
         Master_Interrupt_Controller_Base      : Unsigned_8;
         Slave_Interrupt_Controller_Base       : Unsigned_8;
         Selector_For_Linear_Memory            : Unsigned_16;
         Linear_Address_Of_Stub_Info_Structure : Unsigned_32;
         Linear_Address_Of_Original_Psp        : Unsigned_32;
         Run_Mode                              : Unsigned_16;
         Run_Mode_Info                         : Unsigned_16;
      end record;
   pragma Convention (C, Go32_Info_Block);

   DOS_Extender : Go32_Info_Block;
   pragma Import (C, DOS_Extender, "_go32_info_block");

   --------------------------------------------------------
   -- Handy synomiems for long winded DOS extender names --
   --------------------------------------------------------
   TB_Size : Unsigned_32 renames DOS_Extender.Size_Of_Transfer_Buffer;
   TB_Address : Unsigned_32 renames
     DOS_Extender.Linear_Address_Of_Transfer_Buffer;
   DOS_Ds : Unsigned_16 renames DOS_Extender.Selector_For_Linear_Memory;

   -----------------------------
   -- Mapping DPMI memory     --
   -----------------------------
   type Dpmi_Meminfo is
      record
         Handle  : Unsigned_32;
         Size    : Unsigned_32;
         Address : Unsigned_32;
      end record;
   pragma Convention (C, Dpmi_Meminfo);

   --------------------------------------------------------------------
   function Dpmi_Physical_Address_Mapping (Mapping : Dpmi_Meminfo)
     return Integer;
   pragma Import (C, Dpmi_Physical_Address_Mapping,
                     "__dpmi_physical_address_mapping");

   --------------------------------------------------------------------
   function Dpmi_Allocate_Ldt_Descriptors (Number : Integer)
     return Unsigned_32;
   pragma Import (C, Dpmi_Allocate_Ldt_Descriptors,
                     "__dpmi_allocate_ldt_descriptors");

   --------------------------------------------------------------------
   function Dpmi_Free_Ldt_Descriptor (Number : Unsigned_32)
     return Integer;
   pragma Import (C, Dpmi_Free_Ldt_Descriptor, "__dpmi_free_ldt_descriptor");

   --------------------------------------------------------------------
   function Dpmi_Set_Segment_Base_Address (Selector : Unsigned_32;
                                           Address  : Unsigned_32)
     return Integer;
   pragma Import (C, Dpmi_Set_Segment_Base_Address,
                     "__dpmi_set_segment_base_address");

   --------------------------------------------------------------------
   function Dpmi_Set_Segment_Limit (Selector : Unsigned_32;
                                    Size     : Unsigned_32)
     return Integer;
   pragma Import (C, Dpmi_Set_Segment_Limit, "__dpmi_set_segment_limit");

   ----------------------------------
   -- Selector for current program --
   ----------------------------------
   function My_Ds return Unsigned_32;
   pragma Import (C, My_Ds, "_my_ds");

   --------------------------------
   -- Call a real-mode interrupt --
   --------------------------------
   procedure Dpmi_Int (Vector : in Unsigned_16; Regs : in out Dpmi_Regs);
   pragma Import (C, Dpmi_Int, "__dpmi_int");

   --------------------------------
   -- Move data around in memory --
   --------------------------------

   --------------------------------------------------------------------
   procedure Dosmemget (Offset : in Unsigned_32;
                        Length : in Unsigned_32;
                        Buffer : in Address);
   pragma Import (C, Dosmemget, "dosmemget");

   --------------------------------------------------------------------
   procedure Dosmemput (Buffer : in Address;
                        Length : in Unsigned_32;
                        Offset : in Unsigned_32);
   pragma Import (C, Dosmemput, "dosmemput");

   --------------------------------------------------------------------
   procedure Move_Data_DWords (Source_Selector      : Unsigned_32;
                               Source_Offset        : Address;
                               Destination_Selector : Unsigned_32;
                               Destination_Offset   : Address;
                               Number_Of_DWords     : Integer);
   pragma Import (C, Move_Data_Dwords, "_movedatal");

   -----------------
   -- Fill memory --
   -----------------
   procedure Mem_Set (Buffer : Address;
                      Value  : Integer;
                      Number : Integer);
   pragma Import (C, Mem_Set, "memset");

   -----------------------------
   -- Access real-mode memory --
   -----------------------------

   --------------------------------------------------------------------
   procedure Set_Selector (Selector : in Unsigned_16);
   pragma Import (C, Set_Selector, "_farsetsel");

   --------------------------------------------------------------------
   function Peek_Byte (Offset : Integer) return Unsigned_8 is
      Result : Unsigned_8;
   begin
      ASM (".byte 0x64; movb (%k1), %b0",
        Unsigned_8'Asm_Output ("=q", (result)),
        Integer'Asm_Input ("r", Offset),
        Volatile => True);
      return Result;
   end Peek_Byte;
   pragma Inline (Peek_Byte);

   --------------------------------------------------------------------
   function Peek_Word (Offset : Integer) return Unsigned_16 is
      Result : Unsigned_16;
   begin
      ASM (".byte 0x64; movw (%k1), %w0",
        Unsigned_16'Asm_Output ("=r", (result)),
        Integer'Asm_Input ("r", Offset),
        Volatile => True);
      return Result;
   end Peek_Word;
   pragma Inline (Peek_Word);

   --------------------------------------------------------------------
   procedure Poke_Byte (Offset : in Integer; Value : in Unsigned_8) is
   begin
      ASM (".byte 0x64; movb %b0, (%k1)",
        No_Output_Operands,
        (Unsigned_8'Asm_Input  ("qi", Value),
         Integer'Asm_Input ("r", Offset)),
        Volatile => True);
   end Poke_Byte;
   pragma Inline (Poke_Byte);

   ---------------------------
   -- Access hardware ports --
   ---------------------------

   --------------------------------------------------------------------
   function Inport_Byte (Port : Unsigned_16) return Unsigned_8 is
      Value : Unsigned_8;
   begin
      ASM ("inb %%dx",
        Unsigned_8'Asm_Output ("=a", Value),
        Unsigned_16'Asm_Input ("d", Port),
        Volatile => True);
      return Value;
   end Inport_Byte;
   pragma Inline (Inport_Byte);

   --------------------------------------------------------------------
   procedure Outport_Byte (Port : in Unsigned_16; Value : in Unsigned_8) is
   begin
      ASM ("outb %%dx",
        No_Output_Operands,
        (Unsigned_8'Asm_Input  ("a", Value),
         Unsigned_16'Asm_Input ("d", Port)),
        Volatile => True);
   end Outport_Byte;
   pragma Inline (Outport_Byte);

   -----------------------------------------------------------
   -- Convert a linear adress into its real-mode equivalent --
   -----------------------------------------------------------

   --------------------------------------------------------------------
   function RM_Offset (Address : unsigned_32) return unsigned_16 is
   begin
      return unsigned_16 (Address and 16#0F#);
   end RM_Offset;
   pragma Inline (RM_Offset);

   --------------------------------------------------------------------
   function RM_Segment (Address : unsigned_32) return unsigned_16 is
   begin
      return unsigned_16 (Shift_Right (Address, 4) and 16#FFFF#);
   end RM_Segment;
   pragma Inline (RM_Segment);

   ---------------------------------------------------------
   -- Convert a DWORD real-mode address into a linear one --
   ---------------------------------------------------------
   function RM_To_Linear (Address : unsigned_32) return unsigned_32 is
   begin
      return 16 * Shift_Right (Address, 16) + (Address and 16#FFFF#);
   end RM_To_Linear;
   pragma Inline (RM_To_Linear);

   ------------------------
   -- Package exceptions --
   ------------------------
   VBE_Error : exception;  -- a VBE call failed

   --------------------------------------
   -- Maximum size of a VBE info block --
   --------------------------------------
   Max_Infoblock_Length : constant := 512;

   -------------------
   -- VBE Constants --
   -------------------
   Window_Active       : constant unsigned_8  := 16#07#;
   Window_A            : constant unsigned_16 := 16#0000#;
   Mode_Supported      : constant unsigned_16 := 16#0001#;
   Linear_Mode_Present : constant unsigned_16 := 16#0080#;
   VBE_Call_Ok         : constant unsigned_16 := 16#004F#;
   Linear_Mode_Flag    : constant unsigned_16 := 16#4000#;
   VBE_Signature       : constant unsigned_32 := 16#41534556#;
   VBE_All_States      : constant unsigned_16 := 16#000F#;
   Video_Interrupt     : constant unsigned_16 := 16#0010#;
   Text_Size           : constant unsigned_32 := 2*160*60;
   Text_Address        : constant unsigned_32 := 16#B8000#;

   ----------------------------
   -- VBE Driver information --
   ----------------------------
   type VbeInfoBlock is
      record
         VBESignature : Unsigned_32; -- 4 signature bytes
         VBEVersion   : Unsigned_16; -- VBE version number
         OEMStringPtr : Unsigned_32; -- Pointer to OEM string
         Capabilities : Unsigned_32; -- capabilities of the video environment
         VideoModePtr : Unsigned_32; -- pointer to supported Super VGA modes
         TotalMemory  : Unsigned_16; -- Number of 64kb memory blocks on board
      end record;
   pragma Pack (VbeInfoBlock);

   -----------------------------------
   -- VBE graphics mode information --
   -----------------------------------
   type ModeInfoBlock is
      record
        ModeAttributes      : Unsigned_16; -- mode attributes
        WinAAttributes      : Unsigned_8;  -- window A attributes
        WinBAttributes      : Unsigned_8;  -- window B attributes
        WinGranularity      : Unsigned_16; -- window granularity
        WinSize             : Unsigned_16; -- window size
        WinASegment         : Unsigned_16; -- window A start segment
        WinBSegment         : Unsigned_16; -- window B start segment
        WinFuncPtr          : Unsigned_32; -- pointer to windor function
        BytesPerScanLine    : Unsigned_16; -- bytes per scan line
        XResolution         : Unsigned_16; -- horizontal resolution
        YResolution         : Unsigned_16; -- vertical resolution
        XCharSize           : Unsigned_8;  -- character cell width
        YCharSize           : Unsigned_8;  -- character cell height
        NumberOfPlanes      : Unsigned_8;  -- number of memory planes
        BitsPerPixel        : Unsigned_8;  -- bits per pixel
        NumberOfBanks       : Unsigned_8;  -- number of banks
        MemoryModel         : Unsigned_8;  -- memory model type
        BankSize            : Unsigned_8;  -- bank size in kb
        NumberOfImagePages  : Unsigned_8;  -- number of images
        Reserved            : Unsigned_8;  -- reserved for page function
        RedMaskSize         : Unsigned_8;  -- size direct color red mask
        RedFieldPosition    : Unsigned_8;  -- bit position of LSB of red mask
        GreenMaskSize       : Unsigned_8;  -- size direct color green mask
        GreenFieldPosition  : Unsigned_8;  -- bit position of LSB of green mask
        BlueMaskSize        : Unsigned_8;  -- size direct color blue mask
        BlueFieldPosition   : Unsigned_8;  -- bit position of LSB of blue mask
        RsvdMaskSize        : Unsigned_8;  -- size direct color reserved mask
        RsvdFieldPosition   : Unsigned_8;  -- size direct color reserved mask
        DirectColorModeInfo : Unsigned_8;  -- Direct Color mode attributes
        PhysBasePtr         : Unsigned_32; -- Physical address of frame buffer
        OffScreenMemOffset  : Unsigned_32; -- Pointer to video memory
        OffScreenMemSize    : Unsigned_16; -- Screen memory in 1K units
      end record;
   pragma Pack (ModeInfoBlock);

   ----------------------
   -- VBE State Buffer --
   ----------------------
   type VBE_State is array (Natural range <>) of Unsigned_8;
   pragma Convention (C, VBE_State);

   Supports_State_Buffer: Boolean:= True; -- ATI MACH64 doesn't


   ------------------------
   -- VBE function calls --
   ------------------------

   --------------------------------------------------------------------
   function GetStateSize return Natural is
      Regs : Dpmi_Regs;
   begin
      Regs.Ax := 16#4F04#;
      Regs.Dx := 0;
      Regs.CX := VBE_All_States;
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
      return Natural (Regs.Bx) * 64;
   end GetStateSize;
   pragma Inline (GetStateSize);

   --------------------------------------------------------------------
   function GetVBEMode return Unsigned_16 is
      Regs : Dpmi_Regs;
   begin
      Regs.Ax := 16#4F03#;
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
      return Regs.Bx;
   end GetVBEMode;
   pragma Inline (GetVBEMode);

   --------------------------------------------------------------------
   procedure SetVBEMode (Mode : in Unsigned_16) is
      Regs : Dpmi_Regs;
   begin
      Regs.Ax := 16#4F02#;
      Regs.Bx := Mode;
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
   end SetVBEMode;
   pragma Inline (SetVBEMode);

   --------------------------------------------------------------------
   procedure SaveState (Buffer : out VBE_State) is
      Regs : Dpmi_Regs;
   begin
      if Buffer'Size = 0 then
         raise VBE_Error;
      end if;
      if (Buffer'Size / 8) >= TB_Size then
         raise VBE_Error;
      end if;
      Regs.Ax := 16#4F04#;
      Regs.Dx := 1;
      Regs.CX := VBE_All_States;
      Regs.Es := RM_Segment (TB_Address);
      Regs.Bx := RM_Offset  (TB_Address);
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
      Dosmemget (TB_Address, Buffer'Size / 8, Buffer'Address);
   end SaveState;
   pragma Inline (SaveState);

   --------------------------------------------------------------------
   procedure RestoreState (Buffer : in VBE_State) is
      Regs : Dpmi_Regs;
   begin
      if Buffer'Size = 0 then
         raise VBE_Error;
      end if;
      if (Buffer'Size / 8) >= TB_Size then
         raise VBE_Error;
      end if;
      Dosmemput (Buffer'Address, Buffer'Size / 8, TB_Address);
      Regs.Ax := 16#4F04#;
      Regs.Dx := 2;
      Regs.CX := VBE_All_States;
      Regs.Es := RM_Segment (TB_Address);
      Regs.Bx := RM_Offset  (TB_Address);
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
   end RestoreState;
   pragma Inline (RestoreState);

   --------------------------------------------------------------------
   procedure SetDisplayWindow (Window : in Unsigned_16;
                               Pos    : in Unsigned_16) is
      Regs : Dpmi_Regs;
   begin
      Regs.Ax := 16#4F05#;
      Regs.Bx := Window;
      Regs.Dx := Pos;
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
   end SetDisplayWindow;
   pragma Inline (SetDisplayWindow);

   --------------------------------------------------------------------
   procedure GetVbeInfoBlock (Info : in out VbeInfoBlock) is
      Regs : Dpmi_Regs;
   begin
      if TB_Size < Max_Infoblock_Length then
         raise VBE_Error;
      end if;
      Regs.Ax := 16#4F00#;
      Regs.Es := RM_Segment (TB_Address);
      Regs.Di := RM_Offset (TB_Address);
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
      Dosmemget (TB_Address, Info'Size / 8, Info'Address);
   end GetVbeInfoBlock;
   pragma Inline (GetVbeInfoBlock);

   --------------------------------------------------------------------
   procedure GetModeInfoBlock (Mode : in     Unsigned_16;
                               Info :    out ModeInfoBlock) is
      Regs : Dpmi_Regs;
   begin
      if TB_Size < Max_Infoblock_Length then
         raise VBE_Error;
      end if;
      Regs.Ax := 16#4F01#;
      Regs.Cx := Mode;
      Regs.Es := RM_Segment (TB_Address);
      Regs.Di := RM_Offset (TB_Address);
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
      Dosmemget (TB_Address, Info'Size / 8, Info'Address);
   end GetModeInfoBlock;
   pragma Inline (GetModeInfoBlock);

   --------------------------------------------------------------------
   function GetDisplayWindow (Window : in Unsigned_16) return Unsigned_16 is
      Regs : Dpmi_Regs;
   begin
      Regs.Ax := 16#4F05#;
      Regs.Bx := Window;
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         raise VBE_Error;
      end if;
      return Regs.Dx;
   end GetDisplayWindow;
   pragma Inline (GetDisplayWindow);

   -----------------------
   -- Package constants --
   -----------------------
   Font_Max_Char       : constant Positive := 255;
   Font_Max_Height     : constant Positive := Font_Height - 1;

   ----------------------
   -- Type definitions --
   ----------------------
   type Font_Char is array (0 .. Font_Max_Height) of Unsigned_8;
   type Font_Type is array (0 .. Font_Max_Char) of Font_Char;

   type State_Access is access all VBE_State;
   procedure Free is
     new Ada.Unchecked_Deallocation (VBE_State, State_Access);

   ----------------------
   -- Global variables --
   ----------------------
   Display_Address    : Address;
   Bank_Size          : Natural;
   Line_Size          : Natural;
   Display_Offset     : Natural;
   System_Font        : Font_Type;
   Text_Page          : unsigned_16;
   Text_Cursor        : unsigned_16;
   Text_Rows          : unsigned_16;
   Startup_Mode       : Unsigned_16;
   Current_Bank       : Unsigned_16;
   Prog_Selector      : Unsigned_32;
   Video_Selector     : Unsigned_32;
   State_Buffer       : State_Access;
   Default_Palette    : Color_Palette;
   Linear_Mode        : Boolean := False;
   In_Graphics        : Boolean := False;
   Available_Modes    : Video_Mode_Set;
   Text_Buffer        : String (1 .. Positive (Text_Size));

   --------------------------------
   -- Initialize a screen buffer --
   --------------------------------

   --------------------------------------------------------------------
   procedure Initialize (SB : in out Screen_Buffer) is
      Size : constant Natural := SB.Width * SB.Height;
   begin
      if SB.Data /= null then
         raise Screen_Buffer_Error;
      end if;
      if SB.Width = 0 or SB.Height = 0 or
        SB.Width > X_Loc'Last + 1 or SB.Height > Y_Loc'Last + 1
      then
         raise Out_Of_Range;
      end if;
      SB.Data := new Data_Buffer (0 .. Size - 1);
      Mem_Set (SB.Data.all'Address, 0, Size);
   end Initialize;

   ------------------------------
   -- Finalize a screen buffer --
   ------------------------------

   --------------------------------------------------------------------
   procedure Finalize (SB : in out Screen_Buffer) is
   begin
      Free (SB.Data);
      SB.Data := null;
   end Finalize;

   -------------------------------
   -- Check if in graphics mode --
   -------------------------------

   --------------------------------------------------------------------
   procedure Check_Graphics_Mode is
   begin
      if not In_Graphics then
         raise Not_In_Graphics_Mode;
      end if;
   end Check_Graphics_Mode;
   pragma Inline (Check_Graphics_Mode);

   --------------------------------------------------------
   -- Check if screen coordinates are currently in range --
   --------------------------------------------------------

   --------------------------------------------------------------------
   procedure Check_In_Range (X : in X_Loc; Y : in Y_Loc) is
   begin
      if X > X_Max or Y > Y_Max then
         raise Out_Of_Range;
      end if;
   end Check_In_Range;
   pragma Inline (Check_In_Range);

   --------------------------------------------------------------------
   procedure Check_In_Range (Buffer : in Screen_Buffer;
                             X      : in Natural;
                             Y      : in Natural) is
   begin
      if X >= Buffer.Width or Y >= Buffer.Height then
         raise Out_Of_Buffer_Range;
      end if;
   end Check_In_Range;
   pragma Inline (Check_In_Range);

   --------------------------------
   -- Set display memory address --
   --------------------------------

   --------------------------------------------------------------------
   function Get_Address (X : X_Loc; Y : Y_Loc) return Natural is
      Offset : constant Natural := X + Y * Line_Size;
      Bank   : constant Unsigned_16 := Unsigned_16 (Offset / Bank_Size);
   begin
      if Bank /= Current_Bank then
         SetDisplayWindow (Window_A, Bank);
         Current_Bank := Bank;
      end if;
      return Display_Offset + (Offset mod Bank_Size);
   end Get_Address;
   pragma Inline (Get_Address);

   -------------------------------------------------------------------------
   -- VESA Initialisation : used by Graphics_Mode and Get_available_modes --
   -------------------------------------------------------------------------

   procedure Initialize_Vesa is

      --------------------------------------------------------------
      procedure Load_Video_Modes (Info  : in     VbeInfoBlock;
                                  Modes :    out Video_Mode_Set) is
         Mode_Table   : Unsigned_32;
         Current_Mode : Unsigned_16 := 0;
         Mode_Found   : Boolean := False;
      begin
         Mode_Table := RM_To_Linear (Info.VideoModePtr);
         while Current_Mode /= 16#FFFF# loop
            Current_Mode := Peek_Word (Integer (Mode_Table));
            for I in Video_Mode loop
               if Current_Mode = Video_Mode'Enum_Rep (I) then
                  Modes(I)   := True;
                  Mode_Found := True;
               end if;
            end loop;
            Mode_Table := Mode_Table + 2;
         end loop;
         if not Mode_Found then
            raise No_Valid_Modes;
         end if;
      end Load_Video_Modes;

      Info : VbeInfoBlock;

   begin
      begin
        State_Buffer := new VBE_State (0 .. GetStateSize - 1);
        SaveState (State_Buffer.all);
        Supports_State_Buffer:= True;
      exception
        when VBE_Error => Supports_State_Buffer:= False;
      end;
      Startup_Mode := GetVBEMode;
      Info.VBESignature := 16#45325642#;  -- 'VBE2'
      GetVbeInfoBlock (Info);
      if Info.VBESignature /= VBE_Signature then
         raise No_VBE_Driver;
      end if;
      case Info.VBEVersion is
         when 16#0102# | 16#010A# => Linear_Mode := False;
         when 16#0200# | 16#0201# => Linear_Mode := True;
         when 16#0300#            => Linear_Mode := True;
         when others              => raise Invalid_VBE_Version;
      end case;
      Load_Video_Modes (Info, Available_Modes);
   end Initialize_Vesa;

   -----------------------------
   -- Switch to graphics mode --
   -----------------------------

   --------------------------------------------------------------------
   procedure Graphics_Mode (Mode         : in SVGA_Mode := Autodetect;
                            Force_banked : in Boolean:= False;
                            Use_own_font : in Boolean:= False) is

   -----------------------------------------------------------------

      Own_font: constant Font_type:=
        (
         (  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,126,129,165,129,129,189,153,129,126,  0,  0),
         (  0,  0,  0,126,255,219,255,255,195,231,255,126,  0,  0),
         (  0,  0,  0,  0,108,254,254,254,254,124, 56, 16,  0,  0),
         (  0,  0,  0,  0, 16, 56,124,254,124, 56, 16,  0,  0,  0),
         (  0,  0,  0, 24, 60, 60,231,231,231, 24, 24, 60,  0,  0),
         (  0,  0,  0, 24, 60,126,255,255,126, 24, 24, 60,  0,  0),
         (  0,  0,  0,  0,  0,  0, 24, 60, 60, 24,  0,  0,  0,  0),
         (255,255,255,255,255,255,231,195,195,231,255,255,255,255),
         (  0,  0,  0,  0,  0, 60,102, 66, 66,102, 60,  0,  0,  0),
         (255,255,255,255,255,195,153,189,189,153,195,255,255,255),
         (  0,  0,  0, 30, 14, 26, 50,120,204,204,204,120,  0,  0),
         (  0,  0,  0, 60,102,102,102, 60, 24,126, 24, 24,  0,  0),
         (  0,  0,  0, 63, 51, 63, 48, 48, 48,112,240,224,  0,  0),
         (  0,  0,  0,127, 99,127, 99, 99, 99,103,231,230,192,  0),
         (  0,  0,  0, 24, 24,219, 60,231, 60,219, 24, 24,  0,  0),
         (  0,  0,  0,128,192,224,248,254,248,224,192,128,  0,  0),
         (  0,  0,  0,  2,  6, 14, 62,254, 62, 14,  6,  2,  0,  0),
         (  0,  0,  0, 24, 60,126, 24, 24, 24,126, 60, 24,  0,  0),
         (  0,  0,  0,102,102,102,102,102,102,  0,102,102,  0,  0),
         (  0,  0,  0,127,219,219,219,123, 27, 27, 27, 27,  0,  0),
         (  0,  0,124,198, 96, 56,108,198,198,108, 56, 12,198,124),
         (  0,  0,  0,  0,  0,  0,  0,  0,  0,254,254,254,  0,  0),
         (  0,  0,  0, 24, 60,126, 24, 24, 24,126, 60, 24,126,  0),
         (  0,  0,  0, 24, 60,126, 24, 24, 24, 24, 24, 24,  0,  0),
         (  0,  0,  0, 24, 24, 24, 24, 24, 24,126, 60, 24,  0,  0),
         (  0,  0,  0,  0,  0, 24, 12,254, 12, 24,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0, 48, 96,254, 96, 48,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,192,192,192,254,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0, 40,108,254,108, 40,  0,  0,  0,  0),
         (  0,  0,  0,  0, 16, 56, 56,124,124,254,254,  0,  0,  0),
         (  0,  0,  0,  0,254,254,124,124, 56, 56, 16,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0, 24, 60, 60, 60, 24, 24,  0, 24, 24,  0,  0),
         (  0,102,102,102, 36,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,108,108,254,108,108,108,254,108,108,  0,  0),
         (  0, 24, 24,124,198,194,192,124,  6,134,198,124, 24, 24),
         (  0,  0,  0,  0,  0,194,198, 12, 24, 48,102,198,  0,  0),
         (  0,  0,  0, 56,108,108, 56,118,220,204,204,118,  0,  0),
         (  0, 24, 24, 24, 48,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0, 12, 24, 48, 48, 48, 48, 48, 24, 12,  0,  0),
         (  0,  0,  0, 48, 24, 12, 12, 12, 12, 12, 24, 48,  0,  0),
         (  0,  0,  0,  0,  0,102, 60,255, 60,102,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0, 24, 24,126, 24, 24,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,  0,  0, 24, 24, 24, 48,  0),
         (  0,  0,  0,  0,  0,  0,  0,254,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 24, 24,  0,  0),
         (  0,  0,  0,  2,  6, 12, 24, 48, 96,192,128,  0,  0,  0),
         (  0,  0,  0, 56,108,198,198,214,198,198,108, 56,  0,  0),
         (  0,  0,  0, 24, 56,120, 24, 24, 24, 24, 24,126,  0,  0),
         (  0,  0,  0,124,198,  6, 12, 24, 48, 96,198,254,  0,  0),
         (  0,  0,  0,124,198,  6,  6, 60,  6,  6,198,124,  0,  0),
         (  0,  0,  0, 12, 28, 60,108,204,254, 12, 12, 30,  0,  0),
         (  0,  0,  0,254,192,192,192,252,  6,  6,198,124,  0,  0),
         (  0,  0,  0, 56, 96,192,192,252,198,198,198,124,  0,  0),
         (  0,  0,  0,254,198,  6, 12, 24, 48, 48, 48, 48,  0,  0),
         (  0,  0,  0,124,198,198,198,124,198,198,198,124,  0,  0),
         (  0,  0,  0,124,198,198,198,126,  6,  6, 12,120,  0,  0),
         (  0,  0,  0,  0, 24, 24,  0,  0,  0, 24, 24,  0,  0,  0),
         (  0,  0,  0,  0, 24, 24,  0,  0,  0, 24, 24, 48,  0,  0),
         (  0,  0,  0, 12, 24, 48, 96,192, 96, 48, 24, 12,  0,  0),
         (  0,  0,  0,  0,  0,  0,126,  0,  0,126,  0,  0,  0,  0),
         (  0,  0,  0, 96, 48, 24, 12,  6, 12, 24, 48, 96,  0,  0),
         (  0,  0,  0,124,198,198, 12, 24, 24,  0, 24, 24,  0,  0),
         (  0,  0,  0,124,198,198,222,222,222,220,192,124,  0,  0),
         (  0,  0,  0, 16, 56,108,198,198,254,198,198,198,  0,  0),
         (  0,  0,  0,252,102,102,102,124,102,102,102,252,  0,  0),
         (  0,  0,  0, 60,102,194,192,192,192,194,102, 60,  0,  0),
         (  0,  0,  0,248,108,102,102,102,102,102,108,248,  0,  0),
         (  0,  0,  0,254,102, 98,104,120,104, 98,102,254,  0,  0),
         (  0,  0,  0,254,102, 98,104,120,104, 96, 96,240,  0,  0),
         (  0,  0,  0, 60,102,194,192,192,222,198,102, 58,  0,  0),
         (  0,  0,  0,198,198,198,198,254,198,198,198,198,  0,  0),
         (  0,  0,  0, 60, 24, 24, 24, 24, 24, 24, 24, 60,  0,  0),
         (  0,  0,  0, 30, 12, 12, 12, 12, 12,204,204,120,  0,  0),
         (  0,  0,  0,230,102,108,108,120,108,108,102,230,  0,  0),
         (  0,  0,  0,240, 96, 96, 96, 96, 96, 98,102,254,  0,  0),
         (  0,  0,  0,198,238,254,214,198,198,198,198,198,  0,  0),
         (  0,  0,  0,198,230,246,254,222,206,198,198,198,  0,  0),
         (  0,  0,  0,124,198,198,198,198,198,198,198,124,  0,  0),
         (  0,  0,  0,252,102,102,102,124, 96, 96, 96,240,  0,  0),
         (  0,  0,  0,124,198,198,198,198,198,214,222,124, 14,  0),
         (  0,  0,  0,252,102,102,102,124,108,102,102,230,  0,  0),
         (  0,  0,  0,124,198,198, 96, 56, 12,198,198,124,  0,  0),
         (  0,  0,  0,126,126, 90, 24, 24, 24, 24, 24, 60,  0,  0),
         (  0,  0,  0,198,198,198,198,198,198,198,198,124,  0,  0),
         (  0,  0,  0,198,198,198,198,198,198,108, 56, 16,  0,  0),
         (  0,  0,  0,198,198,198,198,214,214,254,108,108,  0,  0),
         (  0,  0,  0,198,198,198,124, 56,124,198,198,198,  0,  0),
         (  0,  0,  0,102,102,102,102, 60, 24, 24, 24, 60,  0,  0),
         (  0,  0,  0,254,198,140, 24, 48, 96,194,198,254,  0,  0),
         (  0,  0,  0, 60, 48, 48, 48, 48, 48, 48, 48, 60,  0,  0),
         (  0,  0,  0,128,192,224,112, 56, 28, 14,  6,  2,  0,  0),
         (  0,  0,  0, 60, 12, 12, 12, 12, 12, 12, 12, 60,  0,  0),
         ( 16, 56,108,198,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255),
         (  0, 48, 24, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,120, 12,124,204,204,118,  0,  0),
         (  0,  0,  0,224, 96, 96,120,108,102,102,102,124,  0,  0),
         (  0,  0,  0,  0,  0,  0,124,198,192,192,198,124,  0,  0),
         (  0,  0,  0, 28, 12, 12, 60,108,204,204,204,118,  0,  0),
         (  0,  0,  0,  0,  0,  0,124,198,254,192,198,124,  0,  0),
         (  0,  0,  0, 28, 54, 50, 48,124, 48, 48, 48,120,  0,  0),
         (  0,  0,  0,  0,  0,  0,118,204,204,204,124, 12,204,120),
         (  0,  0,  0,224, 96, 96,108,118,102,102,102,230,  0,  0),
         (  0,  0,  0, 24, 24,  0, 56, 24, 24, 24, 24, 60,  0,  0),
         (  0,  0,  0,  6,  6,  0, 14,  6,  6,  6,  6,102,102, 60),
         (  0,  0,  0,224, 96, 96,102,108,120,108,102,230,  0,  0),
         (  0,  0,  0, 56, 24, 24, 24, 24, 24, 24, 24, 60,  0,  0),
         (  0,  0,  0,  0,  0,  0,236,254,214,214,214,214,  0,  0),
         (  0,  0,  0,  0,  0,  0,220,102,102,102,102,102,  0,  0),
         (  0,  0,  0,  0,  0,  0,124,198,198,198,198,124,  0,  0),
         (  0,  0,  0,  0,  0,  0,220,102,102,102,124, 96, 96,240),
         (  0,  0,  0,  0,  0,  0,118,204,204,204,124, 12, 12, 30),
         (  0,  0,  0,  0,  0,  0,220,118,102, 96, 96,240,  0,  0),
         (  0,  0,  0,  0,  0,  0,124,198,112, 28,198,124,  0,  0),
         (  0,  0,  0, 16, 48, 48,252, 48, 48, 48, 54, 28,  0,  0),
         (  0,  0,  0,  0,  0,  0,204,204,204,204,204,118,  0,  0),
         (  0,  0,  0,  0,  0,  0,198,198,198,108, 56, 16,  0,  0),
         (  0,  0,  0,  0,  0,  0,198,198,214,214,254,108,  0,  0),
         (  0,  0,  0,  0,  0,  0,198,108, 56, 56,108,198,  0,  0),
         (  0,  0,  0,  0,  0,  0,198,198,198,198,126,  6, 12,120),
         (  0,  0,  0,  0,  0,  0,254,204, 24, 48,102,254,  0,  0),
         (  0,  0,  0, 14, 24, 24, 24,112, 24, 24, 24, 14,  0,  0),
         (  0,  0,  0, 24, 24, 24, 24, 24, 24, 24, 24, 24,  0,  0),
         (  0,  0,  0,112, 24, 24, 24, 14, 24, 24, 24,112,  0,  0),
         (  0,118,220,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0, 16, 56,108,198,198,254,  0,  0,  0),
         (  0,  0,  0, 60,102,194,192,192,192,194,102, 60, 12,120),
         (  0,  0,  0,204,  0,  0,204,204,204,204,204,118,  0,  0),
         (  0,  0, 12, 24, 48,  0,124,198,254,192,198,124,  0,  0),
         (  0,  0, 16, 56,108,  0,120, 12,124,204,204,118,  0,  0),
         (  0,  0,  0,198,  0,  0,120, 12,124,204,204,118,  0,  0),
         (  0,  0, 96, 48, 24,  0,120, 12,124,204,204,118,  0,  0),
         (  0,  0, 56,108, 56,  0,120, 12,124,204,204,118,  0,  0),
         (  0,  0,  0,  0,  0,  0,124,198,192,192,198,124, 12,120),
         (  0,  0, 16, 56,108,  0,124,198,254,192,198,124,  0,  0),
         (  0,  0,  0,198,  0,  0,124,198,254,192,198,124,  0,  0),
         (  0,  0, 96, 48, 24,  0,124,198,254,192,198,124,  0,  0),
         (  0,  0,  0,102,  0,  0, 56, 24, 24, 24, 24, 60,  0,  0),
         (  0,  0, 24, 60,102,  0, 56, 24, 24, 24, 24, 60,  0,  0),
         (  0,  0, 96, 48, 24,  0, 56, 24, 24, 24, 24, 60,  0,  0),
         (  0,198,  0, 16, 56,108,198,198,254,198,198,198,  0,  0),
         ( 56,108, 56, 16, 56,108,198,198,254,198,198,198,  0,  0),
         ( 12, 24,  0,254,102, 98,104,120,104, 98,102,254,  0,  0),
         (  0,  0,  0,  0,  0,  0,236, 54,118,220,216,110,  0,  0),
         (  0,  0,  0, 62,108,204,204,254,204,204,204,206,  0,  0),
         (  0,  0, 16, 56,108,  0,124,198,198,198,198,124,  0,  0),
         (  0,  0,  0,198,  0,  0,124,198,198,198,198,124,  0,  0),
         (  0,  0, 96, 48, 24,  0,124,198,198,198,198,124,  0,  0),
         (  0,  0, 48,120,204,  0,204,204,204,204,204,118,  0,  0),
         (  0,  0, 96, 48, 24,  0,204,204,204,204,204,118,  0,  0),
         (  0,  0,  0,198,  0,  0,198,198,198,198,126,  6, 12,120),
         (  0,198,  0,124,198,198,198,198,198,198,198,124,  0,  0),
         (  0,198,  0,198,198,198,198,198,198,198,198,124,  0,  0),
         (  0,  0,  0,  0,  0,  0,124,206,222,246,230,124,  0,  0),
         (  0,  0, 56,108,100, 96,240, 96, 96, 96,230,252,  0,  0),
         (  0,  0,  4,124,206,206,214,214,214,230,230,124, 64,  0),
         (  0,  0,  0,  0,  0,198,108, 56, 56,108,198,  0,  0,  0),
         (  0,  0, 14, 27, 24, 24, 24,126, 24, 24, 24,216,112,  0),
         (  0,  0, 12, 24, 48,  0,120, 12,124,204,204,118,  0,  0),
         (  0,  0, 12, 24, 48,  0, 56, 24, 24, 24, 24, 60,  0,  0),
         (  0,  0, 12, 24, 48,  0,124,198,198,198,198,124,  0,  0),
         (  0,  0, 12, 24, 48,  0,204,204,204,204,204,118,  0,  0),
         (  0,  0,  0,118,220,  0,220,102,102,102,102,102,  0,  0),
         (118,220,  0,198,230,246,254,222,206,198,198,198,  0,  0),
         (  0,  0, 60,108,108, 62,  0,126,  0,  0,  0,  0,  0,  0),
         (  0,  0, 56,108,108, 56,  0,124,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0, 48, 48,  0, 48, 48, 96,198,198,124,  0,  0),
         (  0,  0,  0, 56, 68,186,170,178,170,170, 68, 56,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,254,  6,  6,  6,  0,  0,  0),
         (  0,  0, 96,224, 99,102,108, 24, 48,110,195,  6, 12, 31),
         (  0,  0, 96,224, 99,102,108, 26, 54,110,218, 63,  6,  6),
         (  0,  0,  0, 24, 24,  0, 24, 24, 60, 60, 60, 24,  0,  0),
         (  0,  0,  0,  0,  0, 54,108,216,108, 54,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,216,108, 54,108,216,  0,  0,  0,  0),
         ( 17, 68, 17, 68, 17, 68, 17, 68, 17, 68, 17, 68, 17, 68),
         ( 85,170, 85,170, 85,170, 85,170, 85,170, 85,170, 85,170),
         (221,119,221,119,221,119,221,119,221,119,221,119,221,119),
         ( 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24),
         ( 24, 24, 24, 24, 24, 24, 24,248, 24, 24, 24, 24, 24, 24),
         ( 48, 96,192, 16, 56,108,198,198,254,198,198,198,  0,  0),
         ( 56,108,198, 16, 56,108,198,198,254,198,198,198,  0,  0),
         ( 24, 12,  6, 16, 56,108,198,198,254,198,198,198,  0,  0),
         (  0,  0,  0, 56, 68,154,162,162,162,154, 68, 56,  0,  0),
         ( 54, 54, 54, 54, 54,246,  6,246, 54, 54, 54, 54, 54, 54),
         ( 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54),
         (  0,  0,  0,  0,  0,254,  6,246, 54, 54, 54, 54, 54, 54),
         ( 54, 54, 54, 54, 54,246,  6,254,  0,  0,  0,  0,  0,  0),
         (  0,  0, 24, 24,124,198,192,192,198,124, 24, 24,  0,  0),
         (  0,  0,  0,102,102, 60, 24,126, 24,126, 24, 24,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,248, 24, 24, 24, 24, 24, 24),
         ( 24, 24, 24, 24, 24, 24, 24, 31,  0,  0,  0,  0,  0,  0),
         ( 24, 24, 24, 24, 24, 24, 24,255,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,255, 24, 24, 24, 24, 24, 24),
         ( 24, 24, 24, 24, 24, 24, 24, 31, 24, 24, 24, 24, 24, 24),
         (  0,  0,  0,  0,  0,  0,  0,255,  0,  0,  0,  0,  0,  0),
         ( 24, 24, 24, 24, 24, 24, 24,255, 24, 24, 24, 24, 24, 24),
         (  0,  0,  0,118,220,  0,120, 12,124,204,204,118,  0,  0),
         (118,220,  0, 16, 56,108,198,198,254,198,198,198,  0,  0),
         ( 54, 54, 54, 54, 54, 55, 48, 63,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0, 63, 48, 55, 54, 54, 54, 54, 54, 54),
         ( 54, 54, 54, 54, 54,247,  0,255,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,255,  0,247, 54, 54, 54, 54, 54, 54),
         ( 54, 54, 54, 54, 54, 55, 48, 55, 54, 54, 54, 54, 54, 54),
         (  0,  0,  0,  0,  0,255,  0,255,  0,  0,  0,  0,  0,  0),
         ( 54, 54, 54, 54, 54,247,  0,247, 54, 54, 54, 54, 54, 54),
         (  0,  0,  0,  0,  0,198,124,198,198,198,124,198,  0,  0),
         (  0,  0,  0, 52, 24, 44,  6, 62,102,102,102, 60,  0,  0),
         (  0,  0,  0,248,108,102,102,246,102,102,108,248,  0,  0),
         ( 56,108,  0,254,102, 98,104,120,104, 98,102,254,  0,  0),
         (  0,198,  0,254,102, 98,104,120,104, 98,102,254,  0,  0),
         ( 48, 24,  0,254,102, 98,104,120,104, 98,102,254,  0,  0),
         (  0,  0,  0,  0,  0,  0, 56, 24, 24, 24, 24, 60,  0,  0),
         ( 12, 24,  0, 60, 24, 24, 24, 24, 24, 24, 24, 60,  0,  0),
         ( 60,102,  0, 60, 24, 24, 24, 24, 24, 24, 24, 60,  0,  0),
         (  0,102,  0, 60, 24, 24, 24, 24, 24, 24, 24, 60,  0,  0),
         ( 24, 24, 24, 24, 24, 24, 24,248,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0, 31, 24, 24, 24, 24, 24, 24),
         (255,255,255,255,255,255,255,255,255,255,255,255,255,255),
         (  0,  0,  0,  0,  0,  0,  0,255,255,255,255,255,255,255),
         (  0,  0, 24, 24, 24, 24,  0,  0,  0, 24, 24, 24, 24,  0),
         ( 48, 24,  0, 60, 24, 24, 24, 24, 24, 24, 24, 60,  0,  0),
         (255,255,255,255,255,255,255,  0,  0,  0,  0,  0,  0,  0),
         ( 24, 48,  0,124,198,198,198,198,198,198,198,124,  0,  0),
         (  0,  0,  0,120,204,204,204,216,204,198,198,204,  0,  0),
         ( 56,108,  0,124,198,198,198,198,198,198,198,124,  0,  0),
         ( 48, 24,  0,124,198,198,198,198,198,198,198,124,  0,  0),
         (  0,  0,  0,118,220,  0,124,198,198,198,198,124,  0,  0),
         (118,220,  0,124,198,198,198,198,198,198,198,124,  0,  0),
         (  0,  0,  0,  0,  0,  0,102,102,102,102,102,124, 96,192),
         (  0,  0,  0,224, 96, 96,124,102,102,102,102,124, 96,240),
         (  0,  0,  0,240, 96,124,102,102,102,124, 96,240,  0,  0),
         ( 24, 48,  0,198,198,198,198,198,198,198,198,124,  0,  0),
         ( 56,108,  0,198,198,198,198,198,198,198,198,124,  0,  0),
         ( 48, 24,  0,198,198,198,198,198,198,198,198,124,  0,  0),
         (  0,  0, 12, 24, 48,  0,198,198,198,198,126,  6, 12,248),
         ( 12, 24,  0,102,102,102,102, 60, 24, 24, 24, 60,  0,  0),
         (  0,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0, 12, 24, 48,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,254,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0, 24, 24,126, 24, 24,  0,  0,126,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,  0,255),
         (  0,224, 48, 99, 54,236, 24, 54,110,218, 63,  6,  6,  0),
         (  0,  0,  0,127,219,219,219,123, 27, 27, 27, 27,  0,  0),
         (  0,  0,124,198, 96, 56,108,198,198,108, 56, 12,198,124),
         (  0,  0,  0,  0,  0, 24,  0,126,  0, 24,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 24, 12,120),
         (  0, 56,108,108, 56,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,198,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0, 24,  0,  0,  0,  0,  0,  0),
         (  0, 24, 56, 24, 24, 24, 60,  0,  0,  0,  0,  0,  0,  0),
         (  0,120, 12, 56, 12, 12,120,  0,  0,  0,  0,  0,  0,  0),
         (  0, 60,102, 12, 24, 50,126,  0,  0,  0,  0,  0,  0,  0),
         (  0,  0,  0,  0,  0,126,126,126,126,126,126,  0,  0,  0),
         (  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 )
        );

      -----------------------------------------------------------------
      procedure Load_System_Font (Font : out Font_Type) is
         Regs    : Dpmi_Regs;
         Address : Integer;
      begin
         Regs.Ax := 16#1130#;
         Regs.Bx := 16#0200#;
         Dpmi_Int (16#10#, Regs);
         Address := 16 * Integer (Regs.Es) + Integer (Regs.Bp);
         for Num in 0 .. Font_Max_Char loop
            for Byte in 0 .. Font_Max_Height loop
               Font (Num)(Byte) := Peek_Byte (Address);
               Address := Address + 1;
            end loop;
         end loop;
      end Load_System_Font;

      -----------------------------------------------------------------
      procedure Save_Text_Parameters is
         Regs : Dpmi_Regs;
      begin
         Regs.Ax := 16#0F00#;
         Dpmi_Int (Video_Interrupt, Regs);
         Text_Page := Shift_Right (Regs.Bx, 8);
         Regs.Ax := 16#0300#;
         Dpmi_Int (Video_Interrupt, Regs);
         Text_Cursor := Regs.Dx;
         Regs.Ax := 16#1130#;
         Regs.Bx := 0;
         Dpmi_Int (Video_Interrupt, Regs);
         Text_Rows := (Regs.Dx and 16#000000FF#) + 1;
         Dosmemget (Text_Address, Text_Size, Text_Buffer'Address);
      end Save_Text_Parameters;

      The_Mode     : Unsigned_16;
      New_Mode     : Video_Mode;
      DPMI_Info    : Dpmi_Meminfo;
      Current_Mode : ModeInfoBlock;

   begin
      if In_Graphics then
         raise Already_In_Graphics_Mode;
      end if;

      Prog_Selector  := My_Ds;
      Video_Selector := Unsigned_32 (DOS_Ds);
      Set_Selector (DOS_Ds);

      Initialize_Vesa;

      Default_Palette := Get_Palette;

      if Use_own_font then

        System_font:= Own_font;

        --        -- *** Spit the font to a file
        --        declare
        --          xxx: file_type;
        --          package mio is new modular_io(unsigned_8); use mio;
        --        begin
        --          Load_System_Font (System_Font);
        --          create(xxx,out_file,"svga.fnt");
        --            put_line(xxx,"(");
        --          for c in 0 .. Font_Max_Char loop
        --            put(xxx," (");
        --            for l in 0 .. Font_Max_Height loop
        --              put(xxx,System_Font (c)(l));
        --              if l < Font_Max_Height then
        --                put(xxx,",");
        --              else
        --                put(xxx,"),");
        --              end if;
        --            end loop;
        --            new_line(xxx);
        --          end loop;
        --          put(xxx,")");
        --          close(xxx);
        --        end;
      else
        Load_System_Font (System_Font);
      end if;

      if Mode = Autodetect then
         for I in reverse Video_Mode loop
            if Available_Modes(I) = True then
               New_Mode := I;
               exit;
            end if;
         end loop;
      else
         New_Mode := Mode;
         if not Available_Modes(New_Mode) then
            raise Unsupported_Video_Mode;
         end if;
      end if;

      GetModeInfoBlock (SVGA_Mode'Enum_Rep (New_Mode), Current_Mode);
      if Linear_Mode then
         if (Current_Mode.ModeAttributes and Linear_Mode_Present) = 0 or
           Current_Mode.PhysBasePtr = 0 or Force_banked
         then
            Linear_Mode := False;
         end if;
      end if;

      X_Size    := Natural (Current_Mode.XResolution);
      Y_Size    := Natural (Current_Mode.YResolution);
      X_Max     := X_Size - 1;
      Y_Max     := Y_Size - 1;
      Line_Size := Natural (Current_Mode.BytesPerScanLine);

      if not Linear_Mode then
         if (Current_Mode.ModeAttributes and Mode_Supported) = 0 then
            raise Unsupported_Video_Mode;
         end if;
         if (Current_Mode.WinAAttributes and Window_Active) = 0 then
            raise Unsupported_Video_Mode;
         end if;
         Display_Offset  := 16 * Natural (Current_Mode.WinASegment);
         Display_Address := To_Address (Integer_Address (Display_Offset));
         Bank_Size       := 1024 * Natural (Current_Mode.WinGranularity);
      else
         DPMI_Info.Size    := Unsigned_32 (X_Size * Y_Size);
         DPMI_Info.Address := Current_Mode.PhysBasePtr;
         if Dpmi_Physical_Address_Mapping (DPMI_Info) = -1 then
            raise Cannot_Map_Physical_Memory;
         end if;
         Video_Selector := Dpmi_Allocate_Ldt_Descriptors (1);
         if Video_Selector = -1 then
            raise Cannot_Alloc_Local_Desc;
         end if;
         if Dpmi_Set_Segment_Base_Address (Video_Selector,
                                           DPMI_Info.Address) = -1
         then
            raise Cannot_Set_Base_Address;
         end if;
         if Dpmi_Set_Segment_Limit (Video_Selector,
                                    DPMI_Info.Size or 16#fff#) = -1
         then
            raise Cannot_Set_Segment_Limit;
         end if;
         Set_Selector (Unsigned_16 (Video_Selector));
      end if;

      The_Mode := SVGA_Mode'Enum_Rep (New_Mode);
      if Linear_Mode then
         The_Mode := The_Mode or Linear_Mode_Flag;
      end if;

      Save_Text_Parameters;

      SetVBEMode (The_Mode);
      if GetVBEMode /= The_Mode then
         raise Unsupported_Video_Mode;
      end if;

      if not Linear_Mode then
         Current_Bank := GetDisplayWindow (Window_A);
      end if;

      In_Graphics := True;
   end Graphics_Mode;

   -----------------------------------------------
   --  Get available modes (while in text mode) --
   -----------------------------------------------

   --------------------------------------------------------------------
   function Get_available_modes return Video_Mode_Set is
   -----------------------------------------------------------------
   begin
      if In_Graphics then
         raise Already_In_Graphics_Mode;
      end if;

      Prog_Selector  := My_Ds;
      Video_Selector := Unsigned_32 (DOS_Ds);
      Set_Selector (DOS_Ds);

      Initialize_Vesa;

      if Supports_State_Buffer then
         RestoreState (State_Buffer.all);
         Free (State_Buffer);
      end if;

      return Available_modes;

   end Get_available_modes;

   --------------------------------
   -- Report if in graphics mode --
   --------------------------------

   --------------------------------------------------------------------
   function In_Graphics_Mode return Boolean is
   begin
      return In_Graphics = True;
   end In_Graphics_Mode;

   ------------------------------------------------------
   -- Report if Linear Frame Buffer support is enabled --
   ------------------------------------------------------

   --------------------------------------------------------------------
   function Linear_Frame_Buffer return Boolean is
   begin
      return Linear_Mode = True;
   end Linear_Frame_Buffer;

   -------------------------
   -- Return to text mode --
   -------------------------

   --------------------------------------------------------------------
   procedure Text_Mode is
      Regs : Dpmi_Regs;
   begin
      Check_Graphics_Mode;

      if Linear_Mode then
         if Dpmi_Free_Ldt_Descriptor (Video_Selector) = -1 then
            raise Cannot_Free_Local_Desc;
         end if;
      end if;

      if (Startup_Mode and 16#FF00#) = 0 then
         Regs.ax := Startup_Mode;
         Dpmi_Int (Video_Interrupt, Regs);
      else
         SetVBEMode (Startup_Mode);
      end if;

      if Supports_State_Buffer then
         RestoreState (State_Buffer.all);
         Free (State_Buffer);
      end if;

      Restore_Default_Palette;

      if Text_Rows = 43 then
         Regs.Ax := 16#1201#;
         Regs.Bx := 16#0030#;
         Dpmi_Int (Video_Interrupt, Regs);
      elsif Text_Rows = 50 then
         Regs.Ax := 16#1202#;
         Regs.Bx := 16#0030#;
         Dpmi_Int (Video_Interrupt, Regs);
      end if;

      if Text_Rows = 43 or Text_Rows = 50 then
         Regs.Ax := 16#1112#;
         Regs.Bx := 0;
         Dpmi_Int (Video_Interrupt, Regs);
      end if;

      Regs.Ax := 16#0500# or Text_Page;
      Dpmi_Int (Video_Interrupt, Regs);

      Regs.Ax := 16#0200#;
      Regs.Bx := Shift_Left (Text_Page, 8);
      Regs.Dx := Text_Cursor;
      Dpmi_Int (Video_Interrupt, Regs);

      Dosmemput (Text_Buffer'Address, Text_Size, Text_Address);

      In_Graphics := False;
   end Text_Mode;

   -------------------------------
   -- Wait for Vertical Retrace --
   -------------------------------

   --------------------------------------------------------------------
   procedure Wait_For_Vertical_Retrace is
   begin
      while (Inport_Byte (16#03DA#) and 8) = 8 loop
         null;
      end loop;
      while (Inport_Byte (16#03DA#) and 8) = 0 loop
         null;
      end loop;
   end Wait_For_Vertical_Retrace;

   -----------------------------
   -- Restore default palette --
   -----------------------------

   --------------------------------------------------------------------
   procedure Restore_Default_Palette is
   begin
      Set_Palette (Default_Palette);
   end Restore_Default_Palette;

   -----------------------------
   -- Get the current palette --
   -----------------------------

   --------------------------------------------------------------------
   function Get_Palette return Color_Palette is
      Palette : Color_Palette;
   begin
      Outport_Byte (16#3C7#, 0);
      for I in Color_Palette'Range loop
         Palette(I).Red   := Byte (Inport_Byte (16#3C9#));
         Palette(I).Green := Byte (Inport_Byte (16#3C9#));
         Palette(I).Blue  := Byte (Inport_Byte (16#3C9#));
      end loop;
      return Palette;
   end Get_Palette;

   -----------------------------
   -- Set the current palette --
   -----------------------------

   --------------------------------------------------------------------
   procedure Set_Palette (Palette : in Color_Palette) is
   begin
      Outport_Byte (16#3C8#, 0);
      for I in Color_Palette'Range loop
         Outport_Byte (16#3C9#, Unsigned_8 (Palette(I).Red));
         Outport_Byte (16#3C9#, Unsigned_8 (Palette(I).Green));
         Outport_Byte (16#3C9#, Unsigned_8 (Palette(I).Blue));
      end loop;
   end Set_Palette;

   ------------------------------------
   -- Get color from current Palette --
   ------------------------------------

   --------------------------------------------------------------------
   function Get_Color (Color : Color_Type) return RGB_Color is
      Value : RGB_Color;
   begin
      Outport_Byte (16#3C7#, Unsigned_8 (Color));
      Value.Red   := Byte (Inport_Byte (16#3C9#));
      Value.Green := Byte (Inport_Byte (16#3C9#));
      Value.Blue  := Byte (Inport_Byte (16#3C9#));
      return Value;
   end Get_Color;

   ----------------------------------
   -- Set color in current palette --
   ----------------------------------

   --------------------------------------------------------------------
   procedure Set_Color (Color : in Color_Type; Value : in RGB_Color) is
   begin
      Outport_Byte (16#3C8#, Unsigned_8 (Color));
      Outport_Byte (16#3C9#, Unsigned_8 (Value.Red));
      Outport_Byte (16#3C9#, Unsigned_8 (Value.Green));
      Outport_Byte (16#3C9#, Unsigned_8 (Value.Blue));
   end Set_Color;

   ------------------------------
   -- Get the color of a pixel --
   ------------------------------

   --------------------------------------------------------------------
   function Get_Pixel (X : in X_Loc;
                       Y : in Y_Loc) return Color_Type is
   begin
      Check_Graphics_Mode;
      Check_In_Range (X, Y);
      if Linear_Mode then
         return Color_Type (Peek_Byte (X + Y * Line_Size));
      else
         return Color_Type (Peek_Byte (Get_Address (X, Y)));
      end if;
   end Get_Pixel;

   --------------------------------------------------------------------
   function Get_Pixel (Buffer : in Screen_Buffer;
                       X      : in X_Loc;
                       Y      : in Y_Loc) return Color_Type is
   begin
      Check_In_Range (Buffer, X, Y);
      return Buffer.Data(X + Y * Buffer.Width);
   end Get_Pixel;

   ------------------------------
   -- Set the color of a pixel --
   ------------------------------

   --------------------------------------------------------------------
   procedure Set_Pixel (X     : in X_Loc;
                        Y     : in Y_Loc;
                        Color : in Color_Type := Standard_White) is
   begin
      Check_Graphics_Mode;
      Check_In_Range (X, Y);
      if Linear_Mode then
         Poke_Byte (X + Y * Line_Size, Unsigned_8 (Color));
      else
         Poke_Byte (Get_Address (X, Y), Unsigned_8 (Color));
      end if;
   end Set_Pixel;

   --------------------------------------------------------------------
   procedure Set_Pixel (Buffer : in out Screen_Buffer;
                        X      : in     X_Loc;
                        Y      : in     Y_Loc;
                        Color  : in     Color_Type := Standard_White) is
   begin
      Check_In_Range (Buffer, X, Y);
      Buffer.Data(X + Y * Buffer.Width) := Color;
   end Set_Pixel;

   ----------------------------
   -- Draw a horizontal line --
   ----------------------------

   --------------------------------------------------------------------
   procedure Hor_Line (X1, X2 : in X_Loc;
                       Y      : in Y_Loc;
                       Color  : in Color_Type := Standard_White) is
      I           : Natural;
      Left, Right : X_Loc;
   begin
      Check_Graphics_Mode;

      if X1 = X2 then
         Set_Pixel (X1, Y, Color);
      else
         Check_In_Range (X1, Y);
         Check_In_Range (X2, Y);

         Left  := X1;
         Right := X2;

         if X1 > X2 then
            Left  := X2;
            Right := X1;
         end if;

         if Linear_Mode then
            I := Y * Line_Size;
            for X in Left .. Right loop
               Poke_Byte (X + I, Unsigned_8 (Color));
            end loop;
         else
            for X in Left .. Right loop
               Poke_Byte (Get_Address (X, Y), Unsigned_8 (Color));
            end loop;
         end if;
      end if;
   end Hor_Line;

   --------------------------------------------------------------------
   procedure Hor_Line (Buffer :    out Screen_Buffer;
                       X1, X2 : in     X_Loc;
                       Y      : in     Y_Loc;
                       Color  : in     Color_Type := Standard_White) is
      Left, Right : X_Loc;
   begin
      if X1 = X2 then
         Set_Pixel (Buffer, X1, Y, Color);
      else
         Check_In_Range (Buffer, X1, Y);
         Check_In_Range (Buffer, X2, Y);

         Left  := X1;
         Right := X2;

         if X1 > X2 then
            Left  := X2;
            Right := X1;
         end if;

         Mem_Set (Buffer.Data(Left + Y * Buffer.Width)'Address, Integer (Color),
           Right - Left);
      end if;
   end Hor_Line;

   --------------------------
   -- Draw a vertical line --
   --------------------------

   --------------------------------------------------------------------
   procedure Ver_Line (X      : in X_Loc;
                       Y1, Y2 : in Y_Loc;
                       Color  : in Color_Type := Standard_White) is
      Top, Bottom : Y_Loc;
   begin
      Check_Graphics_Mode;

      if Y1 = Y2 then
         Set_Pixel (X, Y1, Color);
      else
         Check_In_Range (X, Y1);
         Check_In_Range (X, Y2);

         Top    := Y1;
         Bottom := Y2;

         if Y1 > Y2 then
            Top    := Y2;
            Bottom := Y1;
         end if;

         if Linear_Mode then
            for Y in Top .. Bottom loop
               Poke_Byte (X + Y * Line_Size, Unsigned_8 (Color));
            end loop;
         else
            for Y in Top .. Bottom loop
               Poke_Byte (Get_Address (X, Y), Unsigned_8 (Color));
            end loop;
         end if;
      end if;
   end Ver_Line;

   procedure Ver_Line (Buffer :    out Screen_Buffer;
                       X      : in     X_Loc;
                       Y1, Y2 : in     Y_Loc;
                       Color  : in     Color_Type := Standard_White) is
      Top, Bottom : Y_Loc;
   begin
      if Y1 = Y2 then
         Set_Pixel (Buffer, X, Y1, Color);
      else
         Check_In_Range (Buffer, X, Y1);
         Check_In_Range (Buffer, X, Y2);

         Top    := Y1;
         Bottom := Y2;

         if Y1 > Y2 then
            Top    := Y2;
            Bottom := Y1;
         end if;

         for I in Top .. Bottom loop
            Buffer.Data(X + I * Line_Size) := Color;
         end loop;
      end if;
   end Ver_Line;

   -----------------
   -- Draw a line --
   -----------------

   --------------------------------------------------------------------
   procedure Line (X1    : in X_Loc;
                   Y1    : in Y_Loc;
                   X2    : in X_Loc;
                   Y2    : in Y_Loc;
                   Color : in Color_Type := Standard_White) is
      Dir    : Boolean := False;
      Left   :          Integer := Integer (X1);
      Right  : constant Integer := Integer (X2);
      Top    :          Integer := Integer (Y1);
      Bottom : constant Integer := Integer (Y2);
      Temp, Limit, D_X, D_Y, S_X, S_Y : Integer;
   begin
      Check_Graphics_Mode;

      if X1 = X2 and Y1 = Y2 then
         Set_Pixel (X1, Y1, Color);
      elsif X1 = X2 then
         Ver_Line (X1, Y1, Y2, Color);
      elsif Y1 = Y2 then
         Hor_Line (X1, X2, Y1, Color);
      else
         Check_In_Range (X1, Y1);
         Check_In_Range (X2, Y2);

         S_X := 1;
         D_X := abs (Right - Left);
         if Left > Right then
            S_X := -1;
         end if;

         S_Y := 1;
         D_Y := abs (Bottom - Top);
         if Top > Bottom then
            S_Y := -1;
         end if;

         if D_Y > D_X then
            Dir  := True;
            Temp := Left;
            Left := Top;
            Top  := Temp;
            Temp := D_X;
            D_X  := D_Y;
            D_Y  := Temp;
            Temp := S_X;
            S_X  := S_Y;
            S_Y  := Temp;
         end if;

         Limit := 2 * D_Y - D_X;

         for I in 0 .. D_X loop

            if Linear_Mode then
               if Dir then
                  Poke_Byte (Top + Left * Line_Size, Unsigned_8 (Color));
               else
                  Poke_Byte (Left + Top * Line_Size, Unsigned_8 (Color));
               end if;
            else
               if Dir then
                  Poke_Byte (Get_Address (Top, Left), Unsigned_8 (Color));
               else
                  Poke_Byte (Get_Address (Left, Top), Unsigned_8 (Color));
               end if;
            end if;

            while Limit >= 0 loop
               Top   := Top + S_Y;
               Limit := Limit - 2 * D_X;
            end loop;

            Left  := Left + S_X;
            Limit := Limit + 2 * D_Y;

         end loop;

         if Linear_Mode then
            Poke_Byte (Right +  Bottom * Line_Size, Unsigned_8 (Color));
         else
            Poke_Byte (Get_Address (Right, Bottom), Unsigned_8 (Color));
         end if;
      end if;
   end Line;

   --------------------------------------------------------------------
   procedure Line (Buffer :    out Screen_Buffer;
                   X1     : in     X_Loc;
                   Y1     : in     Y_Loc;
                   X2     : in     X_Loc;
                   Y2     : in     Y_Loc;
                   Color  : in     Color_Type := Standard_White) is
      Left   : constant Integer := Integer (X1);
      Right  : constant Integer := Integer (X2);
      Top    : constant Integer := Integer (Y1);
      Bottom : constant Integer := Integer (Y2);
      D_X, D_Y, D_X_2, D_Y_2, S_X, S_Y : Integer;
      Limit, Left_Top_Line, Line_S_Y   : Integer;
   begin
      if X1 = X2 then
         if Y1 = Y2 then
            Set_Pixel (Buffer, X1, Y1, Color);
         else
            Ver_Line (Buffer, X1, Y1, Y2, Color);
         end if;
      elsif Y1 = Y2 then
         Hor_Line (Buffer, X1, X2, Y1, Color);
      else
         Check_In_Range (Buffer, X1, Y1);
         Check_In_Range (Buffer, X2, Y2);

         S_X := 1;
         D_X := abs (Right - Left);
         if Left > Right then
            S_X := -1;
         end if;

         S_Y := 1;
         D_Y := abs (Bottom - Top);

         if Top > Bottom then
            S_Y := -1;
         end if;

         D_X_2 := 2*D_X;
         D_Y_2 := 2*D_Y;

         Line_S_Y := Line_Size * S_Y;
         Left_Top_Line := Left + Line_Size * Top;

         if D_Y > D_X then
            Limit := D_X_2 - D_Y;

            for I in 0 .. D_Y loop
               Buffer.Data(Left_Top_Line) := Color;

               while Limit >= 0 loop
                  Left_Top_Line := Left_Top_Line + S_X;
                  Limit := Limit - D_Y_2;
               end loop;

               Left_Top_Line := Left_Top_Line + Line_S_Y;
               Limit := Limit + D_X_2;
            end loop;

         else

            Limit := D_Y_2 - D_X;

            for I in 0 .. D_X loop
               Buffer.Data(Left_Top_Line) := Color;

               while Limit >= 0 loop
                  Left_Top_Line := Left_Top_Line + Line_S_Y;
                  Limit := Limit - D_X_2;
               end loop;

               Left_Top_Line := Left_Top_Line + S_X;
               Limit := Limit + D_Y_2;
            end loop;
         end if;
         Buffer.Data(Right + Line_Size * Bottom) := Color;
      end if;
   end Line;

   ----------------------
   -- Draw a rectangle --
   ----------------------

   procedure Frame_Rect (X1    : in X_Loc;
                         Y1    : in Y_Loc;
                         X2    : in X_Loc;
                         Y2    : in Y_Loc;
                         Color : in Color_Type := Standard_White) is
   begin
     Hor_Line (X1, X2, Y1, Color);
     Hor_Line (X1, X2, Y2, Color);
     Ver_Line (X1, Y1, Y2, Color);
     Ver_Line (X2, Y1, Y2, Color);
   end Frame_Rect;

   procedure Frame_Rect (Buffer :    out Screen_Buffer;
                         X1     : in X_Loc;
                         Y1     : in Y_Loc;
                         X2     : in X_Loc;
                         Y2     : in Y_Loc;
                         Color : in Color_Type := Standard_White) is
   begin
     Hor_Line (Buffer, X1, X2, Y1, Color);
     Hor_Line (Buffer, X1, X2, Y2, Color);
     Ver_Line (Buffer, X1, Y1, Y2, Color);
     Ver_Line (Buffer, X2, Y1, Y2, Color);
   end Frame_Rect;

   ------------------------------------
   -- Draw a visually correct circle --
   ------------------------------------

   --------------------------------------------------------------------
   procedure Circle (X      : in X_Loc;
                     Y      : in Y_Loc;
                     Radius : in Positive;
                     Color  : in Color_Type := Standard_White) is
      Count   : Integer := 0;
      Current : Integer := Radius;
      Diam    : Integer := 2 * (1 - Radius);
      Correct : constant Integer := Integer (2 * X_Size / Y_Size);
   begin
      Check_Graphics_Mode;

      if Radius = 1 then
         Set_Pixel (X, Y, Color);
      else
         Check_In_Range (X + X_Loc (Radius), Y + Y_Loc (Radius));
         Check_In_Range (X + X_Loc (Radius), Y - Y_Loc (Radius));
         Check_In_Range (X - X_Loc (Radius), Y + Y_Loc (Radius));
         Check_In_Range (X - X_Loc (Radius), Y - Y_Loc (Radius));

         while Current >= 0 loop
            if Linear_Mode then
               Poke_Byte (X + Count + (Y + Current) * Line_Size,
                 Unsigned_8 (Color));
               Poke_Byte (X + Count + (Y - Current) * Line_Size,
                 Unsigned_8 (Color));
               Poke_Byte (X - Count + (Y + Current) * Line_Size,
                 Unsigned_8 (Color));
               Poke_Byte (X - Count + (Y - Current) * Line_Size,
                 Unsigned_8 (Color));
            else
               Poke_Byte (Get_Address (X + Count, Y + Current),
                 Unsigned_8 (Color));
               Poke_Byte (Get_Address (X + Count, Y - Current),
                 Unsigned_8 (Color));
               Poke_Byte (Get_Address (X - Count, Y + Current),
                 Unsigned_8 (Color));
               Poke_Byte (Get_Address (X - Count, Y - Current),
                 Unsigned_8 (Color));
            end if;

            if (Diam + Current) > 0 then
               Current := Current - 1;
               Diam := Diam - (Correct * Current) - 1;
            end if;

            if (Count > Diam) then
               Count := Count + 1;
               Diam := Diam + (2 * Count) + 1;
            end if;
         end loop;
      end if;
   end Circle;

   --------------------------------------------------------------------
   procedure Circle (Buffer :    out Screen_Buffer;
                     X      : in     X_Loc;
                     Y      : in     Y_Loc;
                     Radius : in     Positive;
                     Color  : in     Color_Type := Standard_White) is
      Count   : Integer := 0;
      Current : Integer := Radius;
      Diam    : Integer := 2 * (1 - Radius);
      Correct : constant Integer := Integer (2 * X_Size / Y_Size);
   begin
      if Radius = 1 then
         Set_Pixel (Buffer, X, Y, Color);
      else
         Check_In_Range (Buffer, X + X_Loc (Radius), Y + Y_Loc (Radius));
         Check_In_Range (Buffer, X + X_Loc (Radius), Y - Y_Loc (Radius));
         Check_In_Range (Buffer, X - X_Loc (Radius), Y + Y_Loc (Radius));
         Check_In_Range (Buffer, X - X_Loc (Radius), Y - Y_Loc (Radius));

         while Current >= 0 loop

            Buffer.Data(X + Count + Buffer.Width * (Y + Current)) := Color;
            Buffer.Data(X + Count + Buffer.Width * (Y - Current)) := Color;
            Buffer.Data(X - Count + Buffer.Width * (Y + Current)) := Color;
            Buffer.Data(X - Count + Buffer.Width * (Y - Current)) := Color;

            if (Diam + Current) > 0 then
               Current := Current - 1;
               Diam := Diam - (Correct * Current) - 1;
            end if;

            if (Count > Diam) then
               Count := Count + 1;
               Diam := Diam + (2 * Count) + 1;
            end if;
         end loop;
      end if;
   end Circle;

   ------------------------------------------
   -- Draw a mathematically correct circle --
   ------------------------------------------

   --------------------------------------------------------------------
   procedure Math_Circle (X      : in X_Loc;
                          Y      : in Y_Loc;
                          Radius : in Positive;
                          Color  : in Color_Type := Standard_White) is
      X1     : Integer := 0;
      Y1     : Integer := Radius;
      Diff   : Integer := 2 * (1 - Radius);
   begin
      Check_Graphics_Mode;
      if Radius = 1 then
         Set_Pixel (X, Y, Color);
      else
         Check_In_Range (X + X_Loc (Radius), Y + Y_Loc (Radius));
         Check_In_Range (X + X_Loc (Radius), Y - Y_Loc (Radius));
         Check_In_Range (X - X_Loc (Radius), Y + Y_Loc (Radius));
         Check_In_Range (X - X_Loc (Radius), Y - Y_Loc (Radius));

         if Linear_Mode then
            Poke_Byte (X + Radius + Y * Line_Size, Unsigned_8 (Color));
            Poke_Byte (X - Radius + Y * Line_Size, Unsigned_8 (Color));
         else
            Poke_Byte (Get_Address (X + Radius, Y), Unsigned_8 (Color));
            Poke_Byte (Get_Address (X - Radius, Y), Unsigned_8 (Color));
         end if;

         while Y1 > 0 loop

            if Linear_Mode then
               Poke_Byte (X + X1 + (Y + Y1) * Line_Size, Unsigned_8 (Color));
               Poke_Byte (X + X1 + (Y - Y1) * Line_Size, Unsigned_8 (Color));
               Poke_Byte (X - X1 + (Y + Y1) * Line_Size, Unsigned_8 (Color));
               Poke_Byte (X - X1 + (Y - Y1) * Line_Size, Unsigned_8 (Color));
            else
               Poke_Byte (Get_Address (X + X1, Y + Y1), Unsigned_8 (Color));
               Poke_Byte (Get_Address (X + X1, Y - Y1), Unsigned_8 (Color));
               Poke_Byte (Get_Address (X - X1, Y + Y1), Unsigned_8 (Color));
               Poke_Byte (Get_Address (X - X1, Y - Y1), Unsigned_8 (Color));
            end if;

            if Diff + Y1 > 0 then
               Y1 := Y1 - 1;
               Diff := Diff - 2 * Y1 - 1;
            end if;

            if X1 > Diff then
               X1 := X1 + 1;
               Diff := Diff + 2 * X1 + 1;
            end if;
         end loop;
      end if;
   end Math_Circle;

   --------------------------------------------------------------------
   procedure Math_Circle (Buffer :    out Screen_Buffer;
                          X      : in     X_Loc;
                          Y      : in     Y_Loc;
                          Radius : in     Positive;
                          Color  : in     Color_Type := Standard_White) is
      X1     : Integer := 0;
      Y1     : Integer := Radius;
      Diff   : Integer := 2 * (1 - Radius);
   begin
      if Radius = 1 then
         Set_Pixel (Buffer, X, Y, Color);
      else
         Check_In_Range (Buffer, X + X_Loc (Radius), Y + Y_Loc (Radius));
         Check_In_Range (Buffer, X + X_Loc (Radius), Y - Y_Loc (Radius));
         Check_In_Range (Buffer, X - X_Loc (Radius), Y + Y_Loc (Radius));
         Check_In_Range (Buffer, X - X_Loc (Radius), Y - Y_Loc (Radius));

         Buffer.Data(X + Radius + Buffer.Width * Y) := Color;
         Buffer.Data(X - Radius + Buffer.Width * Y) := Color;

         while Y1 > 0 loop

            Buffer.Data(X + X1 + Buffer.Width * (Y + Y1)) := Color;
            Buffer.Data(X + X1 + Buffer.Width * (Y - Y1)) := Color;
            Buffer.Data(X - X1 + Buffer.Width * (Y + Y1)) := Color;
            Buffer.Data(X - x1 + Buffer.Width * (Y - Y1)) := Color;

            if Diff + Y1 > 0 then
               Y1 := Y1 - 1;
               Diff := Diff - 2 * Y1 - 1;
            end if;

            if X1 > Diff then
               X1 := X1 + 1;
               Diff := Diff + 2 * X1 + 1;
            end if;
         end loop;
      end if;
   end Math_Circle;

   ----------------------
   -- Clear the screen --
   ----------------------

   --------------------------------------------------------------------
   procedure Clear_Screen (Color : in Color_Type := Standard_Black) is
      B : Screen_Buffer (X_Size, Y_Size);
   begin
      Check_Graphics_Mode;
      Mem_Set (B.Data.all'Address, Integer (Color), X_Size * Y_Size);
      Put_Buffer (B);
   end Clear_Screen;

   --------------------------------------------------------------------
   procedure Clear_Screen (Buffer : in out Screen_Buffer;
                           Color  : in     Color_Type := Standard_Black) is
   begin
      Mem_Set (Buffer.Data.all'Address, Integer (Color),
               Buffer.Width * Buffer.Height);
   end Clear_Screen;

   ----------------------
   -- Fill a rectangle --
   ----------------------

   --------------------------------------------------------------------
   procedure Fill_Rect (X1    : in X_Loc;
                        Y1    : in Y_Loc;
                        X2    : in X_Loc;
                        Y2    : in Y_Loc;
                        Color : in Color_Type := Standard_White) is
      Left, Right : X_Loc;
      Top, Bottom : Y_Loc;
   begin
      Check_Graphics_Mode;
      if X1 = X2 and Y1 = Y2 then
         Set_Pixel (X1, Y1, Color);
      elsif X1 = X2 then
         Ver_Line (X1, Y1, Y2, Color);
      elsif Y1 = Y2 then
         Hor_Line (X1, X2, Y1, Color);
      else
         Check_In_Range (X1, Y1);
         Check_In_Range (X2, Y2);

         Left := X1; Right := X2;
         if X1 > X2 then
            Left := X2; Right := X1;
         end if;

         Top := Y1; Bottom := Y2;
         if Y1 > Y2 then
            Top := Y2; Bottom := Y1;
         end if;

         for Y in Top .. Bottom loop
            for X in Left .. Right loop
               if Linear_Mode then
                  Poke_Byte (X + Y * Line_Size, Unsigned_8 (Color));
               else
                  Poke_Byte (Get_Address (X, Y), Unsigned_8 (Color));
               end if;
            end loop;
         end loop;
      end if;
   end Fill_Rect;

   --------------------------------------------------------------------
   procedure Fill_Rect (Buffer :    out Screen_Buffer;
                        X1     : in     X_Loc;
                        Y1     : in     Y_Loc;
                        X2     : in     X_Loc;
                        Y2     : in     Y_Loc;
                        Color  : in     Color_Type := Standard_White) is
      Left, Right : X_Loc;
      Top, Bottom : Y_Loc;
   begin
      if X1 = X2 and Y1 = Y2 then
         Set_Pixel (Buffer, X1, Y1, Color);
      elsif X1 = X2 then
         Ver_Line (Buffer, X1, Y1, Y2, Color);
      elsif Y1 = Y2 then
         Hor_Line (Buffer, X1, X2, Y1, Color);
      else
         Check_In_Range (Buffer, X1, Y1);
         Check_In_Range (Buffer, X2, Y2);

         Left := X1; Right := X2;
         if X1 > X2 then
            Left := X2; Right := X1;
         end if;

         Top := Y1; Bottom := Y2;
         if Y1 > Y2 then
            Top := Y2; Bottom := Y1;
         end if;

         for Y in Top .. Bottom loop
            Mem_Set (Buffer.Data(Left + Buffer.Width * Y)'Address,
              Integer (Color), Right - Left);
         end loop;
      end if;
   end Fill_Rect;

   --------------------
   -- Write a string --
   --------------------

   --------------------------------------------------------------------
   procedure Write_String (X     : in X_Loc;
                           Y     : in Y_Loc;
                           S     : in String;
                           Color : in Color_Type := Standard_White) is
      Current : Unsigned_8;
   begin
      Check_Graphics_Mode;
      Check_In_Range (X, Y);
      Check_In_Range (X + S'Length * Font_Width - 1, Y + Font_Max_Height);
      for Count in S'First .. S'Last loop
         for Line in Font_Char'Range loop
            Current := System_Font (Character'Pos (S (Count)))(Line);
            for Bit in 0 .. Font_Width - 1 loop
               if (Current and 16#80#) /= 0 then
                  if Linear_Mode then
                     Poke_Byte (X + Count * Font_Width + Bit +
                       (Y + Line) * Line_Size, Unsigned_8 (Color));
                  else
                     Poke_Byte (Get_Address(X + Count * Font_Width + Bit,
                       Y + Line), Unsigned_8 (Color));
                  end if;
               end if;
               Current := Shift_Left (Current, 1);
            end loop;
         end loop;
      end loop;
   end Write_String;

   --------------------------------------------------------------------
   procedure Write_String (Buffer : in out Screen_Buffer;
                           X      : in     X_Loc;
                           Y      : in     Y_Loc;
                           S      : in     String;
                           Color  : in     Color_Type := Standard_White) is
      Current : Unsigned_8;
   begin
      Check_In_Range (Buffer, X, Y);
      Check_In_Range (Buffer,
        X + S'Length * Font_Width - 1, Y + Font_Max_Height);
      for Count in S'First .. S'Last loop
         for Line in Font_Char'Range loop
            Current := System_Font (Character'Pos (S (Count)))(Line);
            for Bit in 0 .. Font_Width - 1 loop
               if (Current and 16#80#) /= 0 then
                  Buffer.Data(X + Count * Font_Width + Bit +
                    Buffer.Width * (Y + Line)) := Color;
               end if;
               Current := Shift_Left (Current, 1);
            end loop;
         end loop;
      end loop;
   end Write_String;

   -----------------------------------
   -- Copy screen buffer to display --
   -----------------------------------

   --------------------------------------------------------------------
   procedure Put_Buffer (X      : in X_Loc;
                         Y      : in Y_Loc;
                         Buffer : in Screen_Buffer) is
      Index  : Natural := 0;
      X2     : constant X_Loc   := X + X_Loc (Buffer.Width - 1);
      Y2     : constant Y_Loc   := Y + Y_Loc (Buffer.Height - 1);
   begin
      Check_Graphics_Mode;
      if Buffer.Data = null then
         raise Screen_Buffer_Error;
      end if;
      Check_In_Range (X, Y);
      Check_In_Range (X2, Y2);
      for I in Y .. Y2 loop
         for J in X .. X2 loop
            if Linear_Mode then
               Poke_Byte (J + I * Line_Size, Unsigned_8 (Buffer.Data(Index)));
            else
               Poke_Byte (Get_Address (J, I), Unsigned_8 (Buffer.Data(Index)));
            end if;
            Index := Index + 1;
         end loop;
      end loop;
   end Put_Buffer;

   --------------------------------------------------------------------
   procedure Put_Buffer (Buffer : in Screen_Buffer) is

      Size      : Integer;
      Rest      : Integer;
      Num_Banks : Integer;
   begin
      Check_Graphics_Mode;
      if Buffer.Data = null then
         raise Screen_Buffer_Error;
      end if;
      if Buffer.Width /= X_Size and
        Buffer.Height /= Y_Size
      then
         raise Out_Of_Buffer_Range;
      end if;
      Size := Buffer.Data.all'Size / 8;
      if Linear_Mode then
         Move_Data_DWords (Prog_Selector, Buffer.Data.all'Address,
                           Video_Selector, Null_Address, Size / 4);
      else
         Num_Banks := Size / Bank_Size;
         Rest      := Size - Num_Banks * Bank_Size;
         for I in 0  .. Num_Banks - 1 loop
            SetDisplayWindow (Window_A, Unsigned_16 (I));
            Move_Data_DWords (Prog_Selector,
              Buffer.Data(I * Bank_Size)'Address, Video_Selector,
                Display_Address, Bank_Size / 4);
         end loop;
         Current_Bank := Unsigned_16 (Num_Banks);
         SetDisplayWindow (Window_A, Unsigned_16 (Num_Banks));
         if Rest /= 0 then
            Move_Data_DWords (Prog_Selector,
              Buffer.Data(Num_Banks * Bank_Size)'Address, Video_Selector,
                Display_Address, Rest / 4);
         end if;
      end if;
   end Put_Buffer;

   --------------------------------------------------------------------
   procedure Put_Buffer (Source      : in     Screen_Buffer;
                         X           : in     X_Loc;
                         Y           : in     Y_Loc;
                         Destination : in out Screen_Buffer) is
      Index  : Natural := 0;
      I_Line : Natural;
      X2     : constant X_Loc   := X + X_Loc (Source.Width - 1);
      Y2     : constant Y_Loc   := Y + Y_Loc (Source.Height - 1);
   begin
      Check_Graphics_Mode;
      if Source.Data = null then
         raise Screen_Buffer_Error;
      end if;
      Check_In_Range (Destination, X, Y);
      Check_In_Range (Destination, X2, Y2);
      for I in Y .. Y2 loop
         I_Line:= I * Destination.Width;
         for J in X .. X2 loop
            Destination.Data(J + I_Line) :=  Source.Data(Index);
            Index := Index + 1;
         end loop;
      end loop;
   end Put_Buffer;

   procedure Put_Buffer (Source      : in     Screen_Buffer;
                         X           : in     X_Loc;
                         Y           : in     Y_Loc;
                         Destination : in out Screen_Buffer;
                         Transparency: in     Color_Type) is
      Index  : Natural := 0;
      I_Line : Natural;
      X2     : constant X_Loc   := X + X_Loc (Source.Width - 1);
      Y2     : constant Y_Loc   := Y + Y_Loc (Source.Height - 1);
      C      : Color_Type;
   begin
      Check_Graphics_Mode;
      if Source.Data = null then
         raise Screen_Buffer_Error;
      end if;
      Check_In_Range (Destination, X, Y);
      Check_In_Range (Destination, X2, Y2);
      for I in Y .. Y2 loop
         I_Line:= I * Destination.Width;
         for J in X .. X2 loop
            C:= Source.Data(Index);
            if C /= Transparency then
              Destination.Data(J + I_Line) :=  C;
            end if;
            Index := Index + 1;
         end loop;
      end loop;
   end Put_Buffer;

   ----------------------------------------
   -- Copy display area to screen buffer --
   ----------------------------------------

   --------------------------------------------------------------------
   procedure Get_Buffer (X      : in     X_Loc;
                         Y      : in     Y_Loc;
                         Buffer : in out Screen_Buffer) is
      Index  : Natural := 0;
      X2     : constant X_Loc   := X + X_Loc (Buffer.Width - 1);
      Y2     : constant Y_Loc   := Y + Y_Loc (Buffer.Height - 1);
   begin
      Check_Graphics_Mode;
      if Buffer.Data = null then
         raise Screen_Buffer_Error;
      end if;
      Check_In_Range (X, Y);
      Check_In_Range (X2, Y2);
      for I in Y .. Y2 loop
         for J in X .. X2 loop
            if Linear_Mode then
               Buffer.Data(Index) := Byte (Peek_Byte (J + I * Line_Size));
            else
               Buffer.Data(Index) := Byte (Peek_Byte (Get_Address (J, I)));
            end if;
            Index := Index + 1;
         end loop;
      end loop;
   end Get_Buffer;

   --------------------------------------------------------------------
   procedure Get_Buffer (Buffer : out Screen_Buffer) is
      Size      : Integer;
      Rest      : Integer;
      Num_Banks : Integer;
   begin
      Check_Graphics_Mode;

      if Buffer.Data = null then
         raise Screen_Buffer_Error;
      end if;

      if Buffer.Width /= X_Size and
        Buffer.Height /= Y_Size
      then
         raise Out_Of_Buffer_Range;
      end if;
      Size := Buffer.Data.all'Size / 8;

      if Linear_Mode then
         Move_Data_DWords (Video_Selector, Null_Address,
                           Prog_Selector, Buffer.Data.all'Address, Size / 4);
      else
         Num_Banks := Size / Bank_Size;
         Rest      := Size - Num_Banks * Bank_Size;

         for I in 0  .. Num_Banks - 1 loop
            SetDisplayWindow (Window_A, Unsigned_16 (I));
            Move_Data_DWords (Video_Selector, Display_Address, Prog_Selector,
              Buffer.Data(I * Bank_Size)'Address, Bank_Size / 4);
         end loop;

         Current_Bank := Unsigned_16 (Num_Banks);
         SetDisplayWindow (Window_A, Unsigned_16 (Num_Banks));

         if Rest /= 0 then
            Move_Data_DWords (Video_Selector, Display_Address, Prog_Selector,
              Buffer.Data(Num_Banks * Bank_Size)'Address, Rest / 4);
         end if;
      end if;
   end Get_Buffer;


   --------------------------
   -- Copy a screen buffer --
   --------------------------

   procedure Copy_Buffer (Source      : in     Screen_Buffer;
                          Destination : in out Screen_Buffer) is
   begin
      if Source.Data = null or Destination.Data = null then
         raise Screen_Buffer_Error;
      end if;
      if Source.Width /= Destination.Width or
        Source.Height /= Destination.Height
      then
         raise Out_Of_Buffer_Range;
      end if;
      Destination.Data.all := Source.Data.all;
   end Copy_Buffer;

end SVGA;
