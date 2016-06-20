package body SVGA.Effects.Bump is

  granu: constant:= 32;  -- view height as a hand-made fixed-point

  -- Wave equation: {\partial^2}_{t^2} u - \Delta u = 0
  -- Evolution from state k and k-1 to state k+1 following a centered
  -- finite difference scheme in space and time.

  function Wave_Ctr(u_xm1_yp1, u_x_yp1, u_xp1_yp1,
                    u_xm1_y  , u_x_y  , u_xp1_y  ,
                    u_xm1_ym1, u_x_ym1, u_xp1_ym1,
                    u_x_y_tm1                 : Integer ) return Integer is
    mxy: Integer;
    iv_sigma: constant:= 2;
  begin
    mxy:= 2 * u_x_y - u_x_y_tm1 +
          -- centered differences for Laplacian:
           (              u_x_yp1 +
             u_xm1_y  - 4*u_x_y   + u_xp1_y   +
                          u_x_ym1              )  / iv_sigma;

    return mxy - mxy / (granu*2); -- attenuation
  end Wave_Ctr;

  -- Heat equation: \partial_t u - \Delta u = 0
  -- Evolution from state k to state k+1 following a centered finite
  -- difference scheme in space and explicit Euler in time

  function Heat_Ctr(u_xm1_yp1, u_x_yp1, u_xp1_yp1,
                    u_xm1_y  , u_x_y  , u_xp1_y  ,
                    u_xm1_ym1, u_x_ym1, u_xp1_ym1,
                    u_x_y_tm1                 : Integer ) return Integer is
    mxy: Integer;
    iv_sigma: constant:= 4;
  begin
    mxy:= u_x_y +
          -- centered differences for Laplacian:
           (              u_x_yp1 +
             u_xm1_y  - 4*u_x_y   + u_xp1_y   +
                          u_x_ym1              )  / iv_sigma;

    return mxy - mxy / (granu*8); -- attenuation
  end Heat_Ctr;

  -----------
  -- Apply --
  -----------

  -- Internal, with Data_Buffer

  procedure Apply(bump_map :     Height_Map;
                  light_x  :        Integer;
                  light_y  :        Integer;
                  original :     Data_Buffer;
                  deformed : out Data_Buffer
                 ) is

    x1,y1      : Integer;
    dx,dy, d   : Integer;
    c          : Color_type;
    xtot,ytot  : Natural;

    idx, id0y, idxx : Integer; -- index pour (x,y), (0,y+1), (x+1,y)

  begin
    xtot:= bump_map.Width;
    ytot:= bump_map.Height;

    idx := 0;
    for y in 0..Ytot-1 loop
      if y < Ytot-1 then
        id0y:= idx+Xtot;
      else
        id0y:= 0;
      end if;
      for x in 0..Xtot-1 loop
        if x < Xtot-1 then
          idxx:= idx+1;
        else
          idxx:= idx-(Xtot-1); -- pos. de x=0
        end if;

        dx:= (bump_map.Data(idx)-bump_map.Data(idxx)) / granu;
        dy:= (bump_map.Data(idx)-bump_map.Data(x + id0y)) / granu;

        x1:= (x + dx) mod Xtot;
        y1:= (y + dy) mod Ytot;

        d:= dx*light_x + dy*light_y;

        d:= Integer'Max(d, Intensity'First);
        d:= Integer'Min(d, Intensity'Last);

        c:= original( x1 + Xtot * y1 );
        deformed( idx ):= main_intensity_map(c,d);

        idx := idx+1;
      end loop;
    end loop;
  end Apply;

  procedure Apply(bump_map :        Height_Map;
                  light_x  :        Integer;
                  light_y  :        Integer;
                  original :        Screen_Buffer;
                  deformed : in out Screen_Buffer
                 ) is

  begin
    if original.Width /= deformed.Width or
      original.Height /= deformed.Height
    then
       raise Out_Of_Buffer_Range;
    end if;

    if original.Width /= bump_map.Width or
      original.Height /= bump_map.Height
    then
       raise Out_Of_Buffer_Range;
    end if;

    Apply( bump_map, light_x, light_y,
           original.Data.all, deformed.Data.all );
  end Apply;

  procedure Apply(bump_map :        Height_Map;
                  light_x  :        Integer;
                  light_y  :        Integer;
                  original :        Texture_map;
                  deformed : in out Texture_map
                 ) is

  begin
    if original.Width /= deformed.Width or
      original.Height /= deformed.Height
    then
       raise Out_Of_Buffer_Range;
    end if;

    if original.Width /= bump_map.Width or
      original.Height /= bump_map.Height
    then
       raise Out_Of_Buffer_Range;
    end if;

    Apply( bump_map, light_x, light_y,
           original.Data.all, deformed.Data.all );
  end Apply;

  ----------
  -- Blob --
  ----------

  procedure Blob(map        : in out Height_Map;
                 mx,my      : Natural;
                 radius     : Positive;
                 depth      : Integer;
                 boundary   : Boundary_mode )
  is
    r2: constant Positive:= radius*radius;
    n2: Natural;
    xx,yy: Integer;
    xtot,ytot  : Natural;
  begin
    xtot:= map.Width;
    ytot:= map.Height;
    for x in -radius .. +radius loop
      for y in -radius .. +radius loop
        n2:= x*x+y*y;
        if n2 <= r2 then
          xx:= mx+x;
          yy:= my+y;
          if boundary = periodic then
            xx:= xx mod xtot;
            yy:= yy mod ytot;
          end if;
          if xx>= 0 and then xx<= xtot-1 and then
             yy>= 0 and then yy<= ytot-1
          then
            map.Data(xx + yy * xtot):=
              map.Data(xx + yy * xtot) +
                granu * depth * (r2 - n2) / r2 ;
          end if;
        end if;
      end loop;
    end loop;
  end Blob;

  ------------
  -- Evolve --
  ------------

  generic
    with function Scheme( u_xm1_yp1, u_x_yp1, u_xp1_yp1,
                          u_xm1_y  , u_x_y  , u_xp1_y  ,
                          u_xm1_ym1, u_x_ym1, u_xp1_ym1,
                          u_x_y_tm1                 : Integer )
                  return Integer;
  procedure G_Evolve( map_k         : in  Height_Buffer;
                      map_kp1       : out Height_Buffer;
                      xtot,ytot     : Natural;
                      boundary      : Boundary_mode );

  procedure G_Evolve( map_k         : in  Height_Buffer;
                      map_kp1       : out Height_Buffer;
                      xtot,ytot     : Natural;
                      boundary      : Boundary_mode )
  is
    xmax: constant Natural:= xtot-1;
    ymax: constant Natural:= ytot-1;

    procedure Evolve_zero is
      idx: Integer:=0; -- index pour (x,y)
      -- L'algo suppose que le bord de map_k est 0,
      -- mais met a 0 celui de map_kp1 (politesse graphique...)
    begin
      -- \/ BEGIN - Effet sur bord haut / Top-side effect
      for xc in reverse 0..xmax loop -- comptage - optim. x=(xmax)-xc
        map_kp1(idx):= 0;
        idx:= idx+1;
      end loop;
      -- /\ END   - Effet sur bord haut / Top-side effect
      -- \/ BEGIN - Effet sur l'interieur / Effect on interior
      for yc in reverse 1..ytot-2 loop -- comptage - optim. y=(ytot-1)-yc
        map_kp1(idx):= 0; -- Effet sur bord gauche / Left-side effect
        idx:= idx+1;
        for xc in reverse 1..xtot-2 loop -- comptage - optim. x=(xmax)-xc
          map_kp1(idx):=
              Scheme( u_xm1_yp1 => map_k(idx-1+xtot),
                      u_x_yp1   => map_k(idx  +xtot),
                      u_xp1_yp1 => map_k(idx+1+xtot),
                      u_xm1_y   => map_k(idx-1     ),
                      u_x_y     => map_k(idx       ),
                      u_xp1_y   => map_k(idx+1     ),
                      u_xm1_ym1 => map_k(idx-1-xtot),
                      u_x_ym1   => map_k(idx  -xtot),
                      u_xp1_ym1 => map_k(idx+1-xtot),
                      u_x_y_tm1 => map_kp1(idx) );
          idx:= idx+1;
        end loop;
        map_kp1(idx):= 0; -- Effet sur bord droite / Right-side effect
        idx:= idx+1;
      end loop;
      -- /\ END   - Effet sur l'interieur / Effect on interior
      -- \/ BEGIN - Effet sur bord bas / Bottom-side effect
      for xc in reverse 0..xmax loop -- comptage - optim. x=(xmax)-xc
        map_kp1(idx):= 0;
        idx:= idx+1;
      end loop;
      -- /\ END   - Effet sur bord bas / Bottom-side effect
    end Evolve_zero;

    procedure Evolve_periodic is
      idx: Integer:=0; -- index pour (x,y)
      m1x, p1x: Integer;
      m1y, p1y: Integer;
    begin
      for y in 0..ymax loop
        m1y:= -xtot;
        p1y:= +xtot;
        if y=0 then          -- Bord haut / Top-side
          m1y:= ymax * xtot;
        elsif y=ymax then    -- Bord bas  / Bottom-side
          p1y:= -(ymax * xtot);
        end if;
        for x in 0..xmax loop
          m1x:= -1;
          p1x:= +1;
          if x=0 then        -- Bord gauche / Left-side
            m1x:= xmax;
          elsif x=xmax then  -- Bord droit  / Right-side
            p1x:= - xmax;
          end if;
          map_kp1(idx):=
              Scheme( u_xm1_yp1 => map_k(idx+m1x+p1y),
                      u_x_yp1   => map_k(idx    +p1y),
                      u_xp1_yp1 => map_k(idx+p1x+p1y),
                      u_xm1_y   => map_k(idx+m1x    ),
                      u_x_y     => map_k(idx        ),
                      u_xp1_y   => map_k(idx+p1x    ),
                      u_xm1_ym1 => map_k(idx+m1x+m1y),
                      u_x_ym1   => map_k(idx    +m1y),
                      u_xp1_ym1 => map_k(idx+p1x+m1y),
                      u_x_y_tm1 => map_kp1(idx) );
          idx:= idx+1;
        end loop;  -- x
      end loop;  -- y
    end Evolve_periodic;

  begin
    case boundary is
      when     zero =>   Evolve_zero;
      when periodic =>   Evolve_periodic;
    end case;
  end G_Evolve;

  procedure Evolve_Heat_Ctr is new G_Evolve( Scheme => Heat_Ctr );
  procedure Evolve_Wave_Ctr is new G_Evolve( Scheme => Wave_Ctr );

  procedure Evolve( map_k, map_kp1: in out Height_Map;
                    boundary      : Boundary_mode;
                    propagation   : Propagation_mode ) is
  begin
    if map_k.Width  /= map_kp1.Width or
       map_k.Height /= map_kp1.Height
    then
       raise Out_Of_Buffer_Range;
    end if;
    case propagation is
      when heat => Evolve_Heat_Ctr( map_k.Data.all, map_kp1.Data.all,
                                    map_k.Width, map_k.Height,
                                    boundary );
      when wave => Evolve_Wave_Ctr( map_k.Data.all, map_kp1.Data.all,
                                    map_k.Width, map_k.Height,
                                    boundary );
    end case;
  end Evolve;

  ----------------
  -- Initialize --
  ----------------

  -- Picked from Jerry's SVGA.Screen_Buffer !

  procedure Initialize (HM : in out Height_Map) is
      Size : constant Natural := HM.Width * HM.Height;
  begin
    if HM.Data /= null then
       raise Height_Map_Error;
    end if;
    if HM.Width = 0 or HM.Height = 0 or
      HM.Width > X_Loc'Last + 1 or HM.Height > Y_Loc'Last + 1
    then
       raise Out_Of_Range;
    end if;
    HM.Data := new Height_Buffer (0 .. Size - 1);

    for xy in HM.Data'Range loop
      HM.Data(xy):= 0;
    end loop;
  end Initialize;

  --------------
  -- Finalize --
  --------------

  procedure Finalize (HM : in out Height_Map) is
  begin
    Dispose(HM.Data);
    HM.Data := null;
  end Finalize;

end SVGA.Effects.Bump;
