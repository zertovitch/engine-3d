-----------------------------------------------------------------------------
--  File: svgaeffe.adb; see specification (svgaeffe.ads)
-----------------------------------------------------------------------------
--    May-2002: some usage of generics instead of copy-pasting
-- 14-Sep-2003: corrected conversion unsigned <-> negative signed (To_U,To_S)

with Interfaces;                        use Interfaces;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Unchecked_Conversion;

package body SVGA.Effects is

  function To_U is new Ada.Unchecked_Conversion(Sfix,uSfix);
  function To_S is new Ada.Unchecked_Conversion(uSfix,Sfix);
  -- uSfix(Sfix'(-1)) and Sfix(uSfix'(2**31)) must raise Constraint_Error
  -- Two-complement and bit sign are not covered by usual conversions
  -- RM95: 4.6 Type Conversions , Dynamic Semantics, 1.

   procedure Gouraud_Hor_Line
     (Buffer : in out Screen_Buffer;
      X1, X2 : in     Integer;
      Y      : in     Y_Loc;
      C1, C2 : in     Color_Type) is

    Span: Integer:= X2-X1;
    fixColor, DeltaCol: sfix;
    Index: Integer;
    Buffer_Data: constant Data_Buffer_Access:= Buffer.Data;

   begin
    if Span > 0 then

      fixColor:= sfix(C1) * sfix_one;
      Index:= X1 + Y * Buffer.Width;
      DeltaCol:= ((sfix(C2)-sfix(C1)) * sfix_one) / sfix(Span);

      -- Clipping --
      declare Cut: Integer; -- How much to cut at ends
      begin
        Cut:= XLeftClip  - X1;
        if Cut > 0 then -- Clip at end #1
          Span:= Span - Cut;
          Index:= Index + Cut;
          fixColor:= fixColor + DeltaCol * sfix(Cut);
        end if;
        Cut:= X2 - XRightClip;
        if Cut > 0 then -- Clip at end #2
          Span:= Span - Cut;
        end if;
      end;
      -- Clipping now prepared --

      for I in reverse 0 .. Span loop
        Buffer_Data(Index):= Color_type(fixColor / sfix_one);
        Index:= Index + 1;
        fixColor:= fixColor + DeltaCol;
      end loop;

    end if;
   end Gouraud_Hor_Line;

  Texture_Data: Data_Buffer_Access;
  Texture_U_mask:  usfix;
  Texture_V_mask:  usfix;
  uSfx_rep_U, uSfx_rep_V: usfix; -- aff.
  fl_sfix_rep_U, fl_sfix_rep_V:   Real; -- persp.

  procedure Set_Current_texture
     (Texture      : in Texture_map;
      rep_U, rep_V : in Positive ) is
  begin
    Texture_Data:= Texture.Data;
    Texture_U_mask:= Texture.U_mask;
    Texture_V_mask:= Texture.V_mask;
    uSfx_rep_U:= usfix(rep_U);
    uSfx_rep_V:= usfix(rep_V * Texture.width);
    fl_sfix_rep_U:= fl_sfix_one * Real(rep_U);
    fl_sfix_rep_V:= fl_sfix_one * Real(rep_V * Texture.width);
  end Set_Current_texture;

  type Intensity_variation_mode is (var, const, zero);

  -- 14-May-2002:
  --   affine texture mapping line as a generic
  --   procedure instead of the previous copy-pasting-adapting

  type Scanline_direction is (horizontal, vertical);

  generic
    direction: Scanline_direction;
  procedure Affine_TextureMap_Line
     (Buffer         : in out Screen_Buffer;
      X1, X2         : in     Integer;
      Y1, Y2         : in     Integer;
      U1, V1, U2, V2 : in     Sfix;
      int1, int2     : in     Intensity);

  procedure Affine_TextureMap_Line
     (Buffer         : in out Screen_Buffer;
      X1, X2         : in     Integer;
      Y1, Y2         : in     Integer;
      U1, V1, U2, V2 : in     Sfix;
      int1, int2     : in     Intensity) is

    DeltaU, DeltaV, U_a, U_b, V_a, V_b: usfix;
    Index: Integer;
    Color: Color_type;
    int:   Intensity;
    fix_int, Delta_int: sfix;
    Span, Cut: Integer;
    Buffer_Data:  Data_Buffer_Access:= Buffer.Data;
    Buffer_Width: constant Integer:= Buffer.Width;

    -- 13-May-2002:
    --   as a generic procedure instead
    --   of the previous copy-pasting-adapting
    generic
      intens_var_mode: Intensity_variation_mode;
    procedure Draw_Line;

    procedure Draw_Line is
    begin
      for I in reverse 0 .. Span loop
        color:= Texture_Data(Natural(
                       (U_a and Texture_U_mask) or
                       (V_a and Texture_V_mask)
                      ) / sfix_one );
        if color /= transparency_color then

          -- "intens_var_mode" is a generic parameter -> the following
          -- "case" is computed at compile-time and only one "when " choice
          --  appears in the machine code of instanciation,
          --  so there is no test / decision / branch for it inside
          --  of the drawing loop
          case intens_var_mode is

            when var =>
              int:= Intensity (fix_int / sfix_one);
              fix_int:= fix_int + Delta_int;
              if int/=0 then
                color:= main_intensity_map(color,int);
              end if;

            when const =>
              color:= main_intensity_map(color,int1);

            when zero =>
              null;

          end case;

          Buffer_Data(Index):= color;

        end if;

        -- Same remark as for the "case intens_var_mode is...": the following
        -- is computed at compile-time and saves performance so.
        case direction is
          when horizontal => Index:= Index + 1;
          when vertical   => Index:= Index + Buffer_Width;
        end case;

        U_a:= U_a + DeltaU;
        V_a:= V_a + DeltaV;
      end loop;
    end Draw_Line;

    procedure Draw_Line_var is new Draw_Line(var);
    procedure Draw_Line_const is new Draw_Line(const);
    procedure Draw_Line_zero is new Draw_Line(zero);

  begin
    case direction is
      when horizontal => Span:= X2-X1;
      when vertical   => Span:= Y2-Y1;
    end case;

    if Span <= 0 then
      return;
    end if;

    U_a :=  uSfx_rep_U * To_U(U1);
    U_b :=  uSfx_rep_U * To_U(U2);
    V_a :=  uSfx_rep_V * To_U(V1);
    V_b :=  uSfx_rep_V * To_U(V2);
    fix_int:= sfix(int1) * sfix_one;

    Index:= X1 + Y1 * Buffer_Width;
    DeltaU := uSfx_rep_U * To_U((U2-U1) / sfix(Span));
    DeltaV := uSfx_rep_V * To_U((V2-V1) / sfix(Span));
    -- ^ Divisions/Shifts in signed mode to preserve sign!
    Delta_int:= ((sfix(int2)-sfix(int1)) * sfix_one) / sfix(Span);

    -- Clipping --
    case direction is
      when horizontal =>  Cut:= XLeftClip - X1;
      when vertical   =>  Cut:= YTopClip  - Y1;
    end case;

    if Cut > 0 then -- Clip at end #1
      Span:= Span - Cut;
      case direction is
        when horizontal => Index:= Index + Cut;
        when vertical   => Index:= Index + Cut * Buffer_Width;
      end case;
      U_a:= U_a + DeltaU * uSfix(Cut);
      V_a:= V_a + DeltaV * uSfix(Cut);
      fix_int:= fix_int + Delta_int * sfix(Cut);
    end if;
    case direction is
      when horizontal =>  Cut:= X2 - XRightClip;
      when vertical   =>  Cut:= Y2 - YBotClip;
    end case;
    if Cut > 0 then -- Clip at end #2
      Span:= Span - Cut;
    end if;
    -- Clipping now prepared --

    if int1 /= int2 then -- variable_intensity: Gouraud effect
      Draw_Line_var;
    elsif int1/=0 then   -- constant but non-neutral intensity:
      Draw_Line_const;
    else                 -- constant neutral intensity -> raw colour
      Draw_Line_zero;
    end if;

  end Affine_TextureMap_Line;

  -- Both Affine-mapped texture functions are
  -- basically instances of the generic one.

  procedure Affine_TextureMap_Hor_Line
     (Buffer         : in out Screen_Buffer;
      X1, X2         : in     Integer;
      Y              : in     Y_Loc;
      U1, V1, U2, V2 : in     Sfix;
      int1, int2     : in     Intensity) is

    procedure A is new Affine_TextureMap_Line( horizontal );

  begin
    A(Buffer, X1, X2, Y, Y, U1,V1,U2,V2, int1, int2);
  end Affine_TextureMap_Hor_Line;

  procedure Affine_TextureMap_Ver_Line
     (Buffer         : in out Screen_Buffer;
      X              : in     X_Loc;
      Y1, Y2         : in     Integer;
      U1, V1, U2, V2 : in     sfix;
      int1, int2     : in     Intensity) is

    procedure A is new Affine_TextureMap_Line( vertical );

  begin
    A(Buffer, X, X, Y1, Y2, U1,V1,U2,V2, int1, int2);
  end Affine_TextureMap_Ver_Line;

  procedure TextureMap_Hor_Line
     (Buffer             : in out Screen_Buffer;
      X1, X2             : in     Integer;
      Y                  : in     Y_Loc;
      UP1, VP1, UP2, VP2 : in     Real;
      iv_Z1, iv_Z2       : in     Real;
      int1, int2         : in     Intensity) is

    -- In this local model, the focal = 1 (not importing), eye at Z=0

    Span: Integer:= X2-X1;
    UP_sfx_right, VP_sfx_right, UP_sfx, VP_sfx,
    Delta_UP_sfx, Delta_VP_sfx,
    Delta_UP_sfx_granu, Delta_VP_sfx_granu,
    Z, iv_Z, Delta_iv_Z, Delta_iv_Z_granu: Real;
    Index: Integer;
    U, V: usfix;
    U_new, V_new: sfix;  -- signed because of possible <0 values
    DeltaU, DeltaV : usfix;
    Color: Color_type;
    int: Intensity;
    fix_int, Delta_int: sfix;
    Buffer_Data:  Data_Buffer_Access:= Buffer.Data;

    -- Every "granu_persp" pixels, we calculate correct Z, U=Z*UP, V=Z*VP

    -- values:       1 : correct perspective, but it means 1 FPU div per pixel!
    --              >8 : Pentium-friendly (CPU/FPU concurrency)
    --      "infinity" : affine - "swimmy" textures :-(

    granu_persp: constant:= 32;   -- 2 ** something please...
    r_granu_persp: constant Real:= Real(granu_persp);

    -- We pre-increment 1/Z to avoid using Z too short after
    -- the division (CPU // FPU strategy on pentium+)
    procedure Pre_inc_Z(Span_minus_J: Integer) is
    pragma Inline(Pre_Inc_Z);
    begin
      if Span_minus_J >= granu_persp then
        iv_Z:= iv_Z + Delta_iv_Z_granu;
      else
        iv_Z:= iv_Z + Real(Span_minus_J) * Delta_iv_Z;
      end if;
      Z:= 1.0 / iv_Z;  -- <- The mighty division we try to avoid at best
    end Pre_inc_Z;

    -- 13-May-2002: as a generic procedure instead
    --              of the previous copy-pasting-adapting
    generic
      intens_var_mode: Intensity_variation_mode;
    procedure Draw_Line;

    procedure Draw_Line is
    begin
      for Span_minus_I in reverse 0 .. Span loop

        if (Span - Span_minus_I) mod granu_persp = 0 then

          -- It's time to calculate the next perspectively correct U,V
          -- that we will interpolate linearily between

          -- Keep the "old new" as incremented bit-maskable fixed-point coor.
          U:= To_U(U_new);
          V:= To_U(V_new);
          if Span_minus_I > granu_persp then
            UP_sfx:= UP_sfx + Delta_UP_sfx_granu;
            VP_sfx:= VP_sfx + Delta_VP_sfx_granu;
            U_new:= sfix( UP_sfx * Z );
            V_new:= sfix( VP_sfx * Z );
            -- Calculate the deltas for non-projected things:
            DeltaU:= To_U(( U_new - To_S(U) ) / granu_persp);
            DeltaV:= To_U(( V_new - To_S(V) ) / granu_persp);
            -- ^ Divisions/Shifts in signed mode to preserve sign!
            Pre_inc_Z( Span_minus_I - granu_persp );  -- Pre-increment 1/Z.
          elsif Span_minus_I > 0 then  -- Last granular bunch, with >= 1 pixel:
            U_new:= sfix( UP_sfx_right * Z );
            V_new:= sfix( VP_sfx_right * Z );
            -- Calculate the deltas for non-projected things:
            DeltaU:= To_U(( U_new-To_S(U) ) / sfix(Span_minus_I));
            DeltaV:= To_U(( V_new-To_S(V) ) / sfix(Span_minus_I));
            -- ^ Divisions/Shifts in signed mode to preserve sign!
          end if;   -- else: we don't care, it's the last pixel!

        else   -- the affine side of the algo...

          U:= U + DeltaU;
          V:= V + DeltaV;

        end if;

        color:= Texture_Data(Natural(
                         (U and Texture_U_mask) or
                         (V and Texture_V_mask)
                         ) / sfix_one );

        if color /= transparency_color then


          -- "intens_var_mode" is a generic parameter -> the following
          -- "case" is computed at compile-time and only one "when " choice
          --  appears in the machine code of instanciation,
          --  so there is no test / decision / branch for it inside
          --  of the drawing loop
          case intens_var_mode is

            when var =>
              int:= Intensity (fix_int / sfix_one);
              fix_int:= fix_int + Delta_int;
              if int/=0 then
                color:= main_intensity_map(color,int);
              end if;

            when const =>
              color:= main_intensity_map(color,int1);

            when zero =>
              null;

          end case;

          Buffer_Data(Index):= color;

        end if;

        Index:= Index + 1;

      end loop;

    end Draw_Line;

    procedure Draw_Line_var is new Draw_Line(var);
    procedure Draw_Line_const is new Draw_Line(const);
    procedure Draw_Line_zero is new Draw_Line(zero);

  begin
    if Span > 0 then

      UP_sfx :=      UP1 * fl_sfix_rep_U;
      UP_sfx_right:= UP2 * fl_sfix_rep_U;
      VP_sfx :=      VP1 * fl_sfix_rep_V;
      VP_sfx_right:= VP2 * fl_sfix_rep_V;
      iv_Z:=         iv_Z1;
      fix_int:=      sfix(int1) * sfix_one;

      Index:= X1 + Y * Buffer.Width;
      declare delta_pixel: constant Real:= 1.0 / Real(span);
      begin
        Delta_UP_sfx:=  delta_pixel * (UP_sfx_right - UP_sfx);
        Delta_UP_sfx_granu:=  r_granu_persp * Delta_UP_sfx;
        Delta_VP_sfx:=  delta_pixel * (VP_sfx_right - VP_sfx);
        Delta_VP_sfx_granu:=  r_granu_persp * Delta_VP_sfx;
        Delta_iv_Z:=    delta_pixel * (iv_Z2 - iv_Z);
        Delta_iv_Z_granu:= r_granu_persp * Delta_iv_Z;
      end;
      Delta_int:= ((sfix(int2)-sfix(int1)) * sfix_one) / sfix(Span);

      -- Clipping --
      declare Cut: Integer; RCut: Real; -- How much to cut at ends
      begin
        Cut:= XLeftClip  - X1;
        if Cut > 0 then -- Clip at end #1
          Span:= Span - Cut;
          Index:= Index + Cut;
          RCut:= Real(Cut);
          UP_sfx:=  UP_sfx  + Delta_UP_sfx * RCut;
          VP_sfx:=  VP_sfx  + Delta_VP_sfx * RCut;
          iv_Z:=    iv_Z    + Delta_iv_Z   * RCut;
          fix_int:= fix_int + Delta_int    * sfix(Cut);
        end if;

        Z:= 1.0 / iv_Z;

        Cut:= X2 - XRightClip;
        if Cut > 0 then -- Clip at end #2
          Span:= Span - Cut;
          RCut:= Real(Cut);
          UP_sfx_right:=  UP_sfx_right  - Delta_UP_sfx * RCut;
          VP_sfx_right:=  VP_sfx_right  - Delta_VP_sfx * RCut;
        end if;
      end;
      -- Clipping now prepared --

      if Span > 0 then
        -- ^ 14-Sep-2003: cleaner, lack of it caused overflow in U_new:= ...

        -- The trick is that 1/Z, UP=U/Z, VP=V/Z are affine functions of
        -- (XP,YP) = (X/Z, Y/Z), the projected (P) screen coordinates

        U_new:= Sfix( UP_sfx * Z );
        V_new:= Sfix( VP_sfx * Z );

        Pre_inc_Z(Span);

        if int1 /= int2 then -- variable_intensity: Gouraud effect
          Draw_Line_var;
        elsif int1/=0 then   -- constant but non-neutral intensity:
          Draw_Line_const;
        else                 -- constant neutral intensity -> raw colour
          Draw_Line_zero;
        end if;
      end if;

    end if;

  end TextureMap_Hor_Line;

  Phong_vals: constant:= 64;
  subtype Phong_val is Natural range 0..Phong_vals-1;
  type Phong_map is array(Phong_index, Phong_index) of Phong_val;
  the_Phong_Map: Phong_map;

  procedure Calc_Phong_map(map: out Phong_map) is
    f: constant Real:= Real(Phong_val'Last-1);
    p: constant:= 1.6;
  begin
    for i in map'Range(1) loop
      for j in map'Range(2) loop
        map(i,j):=
          Phong_val( (Sin(Real(i)*pi/Real(map'Length(1))) ** p) *
                     (Sin(Real(j)*pi/Real(map'Length(2))) ** p) * f)+1;
      end loop;
    end loop;
  end Calc_Phong_map;

  procedure EnvMap_Hor_Line
     (Buffer              : in out Screen_Buffer;
      X1, X2              : in     Integer;
      Y                   : in     Y_Loc;
      U1, V1, U2, V2      : in     Phong_index;
      Cmin, Num_of_shades : in     Color_type) is

    Span: Integer:= X2-X1;

    DeltaU, DeltaV : sfix;
    Uposfixed, Vposfixed: sfix;
    Index: Integer;
    ph:  Phong_val;
    Buffer_Data: constant Data_Buffer_Access:= Buffer.Data;
  begin
    if Span > 0 then

      Index:= X1 + Y * Buffer.Width;
      Uposfixed:= sfix(U1) * sfix_one;
      Vposfixed:= sfix(V1) * sfix_one;
      DeltaU := (sfix(U2-U1) * sfix_one) / sfix(Span);
      DeltaV := (sfix(V2-V1) * sfix_one) / sfix(Span);

      -- Clipping --
      declare Cut: Integer; -- How much to cut at ends
      begin
        Cut:= XLeftClip  - X1;
        if Cut > 0 then -- Clip at end #1
          Span:= Span - Cut;
          Index:= Index + Cut;
          Uposfixed:= Uposfixed + DeltaU * sfix(Cut);
          Vposfixed:= Vposfixed + DeltaV * sfix(Cut);
        end if;
        Cut:= X2 - XRightClip;
        if Cut > 0 then -- Clip at end #2
          Span:= Span - Cut;
        end if;
      end;
      -- Clipping now prepared --

      for I in reverse 0 .. Span loop
        ph:= the_Phong_map( Natural(Uposfixed / sfix_one),
                            Natural(Vposfixed / sfix_one) );
        Buffer_Data(Index):= Cmin +
         Color_type( (Natural(Num_of_shades) * Natural(ph)) / Phong_vals );

        Index:= Index + 1;
        Uposfixed:= Uposfixed + DeltaU;
        Vposfixed:= Vposfixed + DeltaV;

      end loop;

    end if;
  end EnvMap_Hor_Line;

  procedure Reset_main_intensity_map is
  begin
    for c in Color_type loop
      for i in Intensity loop
        main_intensity_map(c,i):= c; -- Each colour is independant
      end loop;
    end loop;
  end Reset_main_intensity_map;

  procedure Calculate_main_intensity_map(d: Degrade_description) is
    ca, cb: Color_type;
    fca, fcb, slope, fci, iv_slope: Real;
  begin
    Reset_main_intensity_map;
    for i in d'Range loop
      ca:= d(i).c;
      cb:= ca + Color_type( abs(d(i).n) );
      fca:= Real(ca);
      fcb:= Real(cb);
      slope:= Real(Intensity'Last) / Real( d(i).n );
      -- slope of an equi-colour level line
      if cb > ca then -- 1-colour d\'egrad\'e is let resetted
        iv_slope:= 1.0 / slope;
        for c in ca .. cb loop
          for int in Intensity loop
            -- we use the fact that main_intensity_map(c,0) must be = c
           fci:= Real'Floor( 0.5 + Real(c) + Real(int) * iv_slope );
           if fci >= fcb then fci:= fcb;
           elsif fci <= fca then fci:= fca; end if; -- ci outside the slide
           main_intensity_map(c,int):= Color_Type(fci);
          end loop;
        end loop;
      end if;
    end loop;
  end Calculate_main_intensity_map;

  function Get_main_intensity_map return Intensity_map is -- 2-Aug-2001
  begin
    return main_intensity_map;
  end;

  procedure Set_Pixel(t: in out Texture_map; x,y: Natural; Color: Color_Type) is
  begin
    t.Data( x + y * t.width ):= Color;
  end;

  procedure Put_Buffer (Source      : in     Screen_Buffer;
                        Destination : in out Texture_map) is
  begin
    if Source.width  /= Destination.width  or else
       Source.height /= Destination.height then
      raise Out_of_range;
    end if;
    Destination.data.all:= Source.data.all;
  end Put_Buffer;

  procedure Initialize (TM : in out Texture_map) is
     use Interfaces;
     Size : Natural;
     Oversized_texture: exception;
  begin
     if TM.x_bits + TM.y_bits + Sfix_bits  > 30 then
       raise Oversized_texture;
     end if;
     TM.width:=   2**TM.x_bits;
     TM.height:=  2**TM.y_bits;
     Size:= TM.width * TM.height;
     TM.U_mask:= Unsigned_32( TM.width  - 1)               * sfix_one;
     TM.V_mask:= Unsigned_32((TM.height - 1) * (TM.width)) * sfix_one;
     if TM.Data /= null then
        raise Screen_Buffer_Error;
     end if;
     TM.Data := new Data_Buffer(0 .. Size - 1);
  end Initialize;

  procedure Finalize (TM : in out Texture_map) is
  begin
     Free (TM.Data);
     TM.Data := null;
  end Finalize;

begin
  Reset_main_intensity_map;
  Calc_Phong_map(the_Phong_map);
end SVGA.Effects;
