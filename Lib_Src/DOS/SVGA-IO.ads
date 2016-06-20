------------------------------------------------------------------------------
--  File:            svga-io.ads
--  Description:     I/O for SVGA graphics package
--  Date / Version:  7-Feb-2000
--  Author:          Gautier de Montmollin
--  Contributions:   Bob Sutton, PLCWestern(~)aol.com
------------------------------------------------------------------------------
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;

package SVGA.IO is

  --------------------
  -- BMP format I/O --
  --------------------

  -- Read picture dimensions from a BMP file
  procedure Read_BMP_Header (Name:      String;
                             width:  out X_Loc;
                             height: out Y_Loc);

  -- Old name for the same thing (compatibility, HAS disappeared!)
--   procedure Read_BMP_Dimensions(Name:      String;
--                                 width:  out X_Loc;
--                                 height: out Y_Loc ) renames Read_BMP_Header;

  -- Read picture header from a BMP stream
  procedure Read_BMP_Header (S: Stream_Access;
                             width:  out X_Loc;
                             height: out Y_Loc);

  -- Load palette only, from BMP file
  -- This reads RGB Quad Structure (BGR order)

  procedure Load_BMP_Palette (S       : Stream_Access;
                              Palette : out Color_Palette);

  -- Load palette only from BMP file

  procedure Load_BMP_Palette (Name:     String;
                              Palette:  out Color_Palette);

  -- Load image only from stream (after having read header and palette!)
  --   The reason for this procedure is that creation of buffer may
  --   follow the reading of header and precede the reading of image!

  procedure Load_BMP_Image (S       : Stream_Access;
                            width   : in X_Loc;
                            height  : in Y_Loc;
                            Buffer  : in out Screen_Buffer);

  -- Load the contents of full BMP file to buffer & palette
  procedure Load_BMP (Name:     String;
                      Buffer:   in out Screen_Buffer;
                      Palette:  out Color_Palette);

  -- Load the contents of full BMP from a stream to buffer & palette
  procedure Load_BMP (S       : Stream_Access;
                      Buffer  : in out Screen_Buffer;
                      Palette : out Color_Palette );

  -- Load the contents of BMP file to screen
  procedure Load_BMP (Name: String);

  -- Save a buffer & palette, in BMP format, as file
  procedure Save_BMP (Name:     String;
                      Buffer:   Screen_Buffer;
                      Palette:  Color_Palette);

  -- Save a screen, in BMP format, as file
  procedure Save_BMP (Name: String);


  -- SVGA.IO exceptions

  Image_too_large,
  Image_X_offset_too_great,
  Image_Y_offset_too_great,
  Unsupported_BMP_format,
  Not_BMP_format, Broken_file,
  Unsupported_bits_per_pixel,
  Unsupported_compression:      exception;

  ----------------------
  -- GIF format Input --
  ----------------------

  -- Load the contents of GIF file to buffer & palette

  procedure Load_GIF (Name:     String;
                      Buffer:   in out Screen_Buffer;
                      Palette:  out Color_Palette);

  -- Load the contents of GIF file to screen

  procedure Load_GIF (Name: String);

  Not_GIF_format, Unknown_GIF_separator: exception;

end SVGA.IO;
