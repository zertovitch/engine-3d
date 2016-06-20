------------------------------------------------------------------------------
--  File:            svgeffio.ads
--  Description:     I/O for SVGA.Effects - in fact, for textures
--  Date / Version:  8-Feb-2000
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;

package SVGA.Effects.IO is

   procedure Load_BMP_texture(S: Stream_Access; Texture: out p_Texture_map);

end SVGA.Effects.IO;
