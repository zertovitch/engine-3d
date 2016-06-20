with SVGA.IO;

package body SVGA.Effects.IO is

  procedure Load_BMP_texture(S: Stream_Access; Texture: out p_Texture_map) is
    width: X_Loc; height: Y_Loc;

    function bits(n: Natural) return Natural is
      nn: Natural:= n; b: Natural:= 0;
    begin
      while nn/=0 loop b:= b+1; nn:= nn / 2; end loop;
      return b;
    end;

  begin
    SVGA.IO.Read_BMP_Header( S, width, height );
    declare
      B: Screen_Buffer( width, height );
      P: Color_Palette; -- unused here
    begin
      texture:= new Texture_map( bits(width-1), bits(height-1) );
      SVGA.IO.Load_BMP_Palette( S, P );
      SVGA.IO.Load_BMP_Image( S, width, height, B );
      Put_Buffer( B, Texture.all );
    end;
  end Load_BMP_texture;

end SVGA.Effects.IO;
