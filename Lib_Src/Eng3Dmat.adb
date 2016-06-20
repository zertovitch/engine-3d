-----------------------------------------------------------------------------
--  File: eng3dmat.adb; see specification (eng3dmat.ads)
-----------------------------------------------------------------------------

with SVGA.Effects;

package body Engine_3D.Math is

  use SVGA.Effects.El_Func; -- G.E.F. for sqrt,sin,cos,etc. on type 'Real'

  -- The fixed-point is home-made, so are the floor and ceiling functions
  function Floor(f:fix) return Integer is
  begin return f / fix_one; end;

  function Ceiling(f:fix) return Integer is
  begin
    if f mod fix_one = 0 then
      return f / fix_one;
    else
      return 1 + f / fix_one;
    end if;
  end;

  -- Vectors & matrices

  function To_RealPoint(p: Point) return RealPoint is
  begin return (Real(p.x),Real(p.y),Real(p.z)); end;

  function To_RealPoint(p: Fixed_Point) return RealPoint is
  begin return (Real(p.x/fix_one),Real(p.y/fix_one),Real(p.z/fix_one)); end;

  function To_point(p: RealPoint) return Point is
  begin return (Integer(p(1)),Integer(p(2)),Integer(p(3))); end;

  function To_fixed_point(p: RealPoint) return Fixed_Point is
  begin
    return ( x=> Fix( fl_fix_one * p(1) ),
             y=> Fix( fl_fix_one * p(2) ),
             z=> Fix( fl_fix_one * p(3) ) );
  end To_fixed_point;

  function To_Vector3(v: Fixed_vectT) return Vector3 is
  begin
    return (1=> Real(v.x/fix_one),
            2=> Real(v.y/fix_one),
            3=> Real(v.z/fix_one));
  end To_Vector3;

  function "*"(l:Real; v:Vector3) return Vector3 is
  begin
    return (l*v(1),l*v(2),l*v(3));
  end "*";

  function "+"(a,b:Vector3) return Vector3 is
  begin
    return (a(1)+b(1),a(2)+b(2),a(3)+b(3));
  end "+";

  function "-"(a,b:Vector3) return Vector3 is
  begin
    return (a(1)-b(1),a(2)-b(2),a(3)-b(3));
  end "-";

  function "-"(a:Vector3) return Vector3 is
  begin
    return (-a(1),-a(2),-a(3));
  end "-";

  function "*"(a,b:Vector3) return Real is    -- dot product
  begin
    return a(1)*b(1)+a(2)*b(2)+a(3)*b(3);
  end "*";

  -- 17-Jul-2001 :
  -- We return an altered dot product according to the material.
  -- N1 is normed, V2 is not. Neutral output: (N1|V2)

  reflexion_exponents: constant array( Reflexion_type ) of Real :=
    ( very_matt   => 1.0/4.0,
      matt        => 1.0/2.0,
      neutral     => 1.0,
      bright      => 2.0,
      very_bright => 4.0 );

  function Special_dot( N1,V2: Vector3; effect: Reflexion_type ) return Real
  is
    l2, dot, valdot, trafiquoval, expo, compensation: Real;
  begin
    if effect=neutral then
      return N1*V2;
    else
      l2:= Norm(V2);
      if l2 < Real'Epsilon then -- !!sort out what orthodox "95" epsilon is
        return 0.0;              -- V2 ~= 0 ~-> N1*V2 ~=0
      else
        dot:= N1 * ( (1.0/l2) * V2 );
        -- the true dot, between normed vectors
        valdot:= abs(dot);
        -- must be in [0;1], thanks to Cauchy-Schwarz
        expo:= reflexion_exponents(effect);
        compensation:= 0.5 * (expo+1.0); -- 1/(2*intregal(0;1;x**expo;dx))
        trafiquoval:= (valdot ** expo) * compensation;
        -- ^ to see the effect, trace the graph of x**expo for x in [0;1]
        return Sign(dot) * l2 * trafiquoval;
      end if;
    end if;
  end Special_dot;

  function "*"(a,b:Vector3) return Vector3 is -- cross product
  begin
    return ( a(2)*b(3) - a(3)*b(2),
             a(3)*b(1) - a(1)*b(3),
             a(1)*b(2) - a(2)*b(1) );
  end;  --                   ^-------- Arrgh! There was a '-' there
        -- since 6-Apr-1999 (004). Fixed 16-Jul-2001

  function Norm(a: Vector3) return Real is
  begin return Sqrt(a(1)*a(1)+a(2)*a(2)+a(3)*a(3)); end;

  function Norm2(a: Vector3) return Real is
  begin return a(1)*a(1)+a(2)*a(2)+a(3)*a(3); end;

  function "+"(a,b: Fixed_vectT) return Fixed_vectT is
  begin
    return ( a.x+b.x, a.y+b.y, a.z+b.z );
  end "+";

  function "*"(A,B: FixedMatrix33) return FixedMatrix33 is
    r: Fix; AB: FixedMatrix33;
  begin
    for i in 1..3 loop
      for j in 1..3 loop
        r:= 0;
        for k in 1..3 loop
          r:= r + (A(i,k) * B(k,j)) / fix_one; -- because 1*1 = 1 ;-)
        end loop;
        AB(i,j):= r;
      end loop;
    end loop;
    return AB;
  end "*";

  function "*"(A,B: Matrix33) return Matrix33 is
    r: Real; AB: Matrix33;
  begin
    for i in 1..3 loop
      for j in 1..3 loop
        r:= 0.0;
        for k in 1..3 loop
          r:= r + (A(i,k) * B(k,j));
        end loop;
        AB(i,j):= r;
      end loop;
    end loop;
    return AB;
  end "*";

  function Transpose(A:Matrix33) return Matrix33 is
  begin
    return ( (a(1,1),a(2,1),a(3,1)),
             (a(1,2),a(2,2),a(3,2)),
             (a(1,3),a(2,3),a(3,3)));
  end Transpose;

  function Det(A:Matrix33) return Real is
  begin
    return
      a(1,1) * a(2,2) * a(3,3) +
      a(2,1) * a(3,2) * a(1,3) +
      a(3,1) * a(1,2) * a(2,3) -
      a(3,1) * a(2,2) * a(1,3) -
      a(2,1) * a(1,2) * a(3,3) -
      a(1,1) * a(3,2) * a(2,3);
  end Det;

  -- Warning: 1) slow method! 2) useless for rotations and, more
  -- generally, orthogonal matrices

  function Inverse(A:Matrix33) return Matrix33 is
    B: Matrix33;
    i1,i2,j1,j2: Integer;
    idet_A, det_mineur: Real;
  begin
    idet_A:= 1.0 / Det(A);
    for i in 1..3 loop
      i1:= ((i-1) - 1) mod 3 + 1;
      i2:= ((i+1) - 1) mod 3 + 1;
      for j in 1..3 loop
        j1:= ((j-1) - 1) mod 3 + 1;
        j2:= ((j+1) - 1) mod 3 + 1;
        det_mineur:= a(i1,j1) * a(i2,j2) -
                     a(i1,j2) * a(i2,j1);
        B(j,i):= idet_A * det_mineur;
        -- sans "* (-1.0)**(i+j)" car i,j = 1 ou 3 permute
        -- le mineur et fait un "* (-1.0) ** impair"
      end loop;
    end loop;
    return B;
  end Inverse;

  function "*"(A:matrix33; x:Vector3) return Vector3 is
    r: Real;
    Ax: Vector3;
  begin
    for i in 1..3 loop
      r:= 0.0;
      for j in 1..3 loop
          r:= r + A(i,j) * x(j);
      end loop;
      Ax(i):= r;
    end loop;
    return Ax;
  end "*";

  -- Following procedure is from Project Spandex, http://www.grafix3d.tzo.com/
  procedure Orthonormalize(M: in out Matrix33) is
    dot1,dot2,vlen: Real;
  begin
    dot1:= m(1,1) * m(2,1) + m(1,2) * m(2,2) + m(1,3) * m(2,3);
    dot2:= m(1,1) * m(3,1) + m(1,2) * m(3,2) + m(1,3) * m(3,3);

    m(1,1) := m(1,1) - dot1 * m(2,1) - dot2 * m(3,1);
    m(1,2) := m(1,2) - dot1 * m(2,2) - dot2 * m(3,2);
    m(1,3) := m(1,3) - dot1 * m(2,3) - dot2 * m(3,3);

    vlen:= 1.0 / sqrt(m(1,1) * m(1,1) +
                      m(1,2) * m(1,2) +
                      m(1,3) * m(1,3));

    m(1,1):= m(1,1) * vlen;
    m(1,2):= m(1,2) * vlen;
    m(1,3):= m(1,3) * vlen;

    dot1:= m(2,1) * m(1,1) + m(2,2) * m(1,2) + m(2,3) * m(1,3);
    dot2:= m(2,1) * m(3,1) + m(2,2) * m(3,2) + m(2,3) * m(3,3);

    m(2,1) := m(2,1) - dot1 * m(1,1) - dot2 * m(3,1);
    m(2,2) := m(2,2) - dot1 * m(1,2) - dot2 * m(3,2);
    m(2,3) := m(2,3) - dot1 * m(1,3) - dot2 * m(3,3);

    vlen:= 1.0 / sqrt(m(2,1) * m(2,1) +
                      m(2,2) * m(2,2) +
                      m(2,3) * m(2,3));

    m(2,1):= m(2,1) * vlen;
    m(2,2):= m(2,2) * vlen;
    m(2,3):= m(2,3) * vlen;

    m(3,1):= m(1,2) * m(2,3) - m(1,3) * m(2,2);
    m(3,2):= m(1,3) * m(2,1) - m(1,1) * m(2,3);
    m(3,3):= m(1,1) * m(2,2) - m(1,2) * m(2,1);
  end Orthonormalize;

  function To_Real(FM: FixedMatrix33) return Matrix33 is
    RM: Matrix33;
  begin
    for i in FM'Range(1) loop
      for j in FM'Range(2) loop
        RM(i,j):= Real(FM(i,j)) * iv_fl_fix_one;
      end loop;
    end loop;
    return RM;
  end To_Real;

  function To_Fix(RM: Matrix33) return FixedMatrix33 is
    FM: FixedMatrix33;
  begin
    for i in RM'Range(1) loop
      for j in RM'Range(2) loop
        FM(i,j):= Fix( fl_fix_one * RM(i,j) );
      end loop;
    end loop;
    return FM;
  end To_Fix;

  function XYZ_rotation(ax,ay,az: Real) return FixedMatrix33 is
    Mx, My, Mz: FixedMatrix33; c,s: Fix;
  begin
    -- Around X
    c:= fix( fl_fix_one * cos( ax ) );
    s:= fix( fl_fix_one * sin( ax ) );
    Mx:= ( (fix_one,0,0),  (0, c , -s ),   (0, s, c ) );
    -- Around Y
    c:= fix( fl_fix_one * cos( ay ) );
    s:= fix( fl_fix_one * sin( ay ) );
    My:= ( ( c ,0, -s ),  (0,fix_one,0),   ( s,0, c ) );
    -- Around Z
    c:= fix( fl_fix_one * cos( az ) );
    s:= fix( fl_fix_one * sin( az ) );
    Mz:= ( ( c , -s ,0),  ( s, c ,0),   (0,0,fix_one) );

    return Mz * My * Mx;
  end XYZ_rotation;

  function XYZ_rotation(ax,ay,az: Real) return Matrix33 is
    Mx, My, Mz: Matrix33; c,s: Real;
  begin
    -- Around X
    c:= cos( ax );
    s:= sin( ax );
    Mx:= ( (1.0,0.0,0.0),  (0.0, c , -s ),   (0.0, s, c ) );
    -- Around Y
    c:= cos( ay );
    s:= sin( ay );
    My:= ( ( c ,0.0, -s ),  (0.0,1.0,0.0),   ( s,0.0, c ) );
    -- Around Z
    c:= cos( az );
    s:= sin( az );
    Mz:= ( ( c , -s ,0.0),  ( s, c ,0.0),   (0.0,0.0,1.0) );

    return Mz * My * Mx;
  end XYZ_rotation;

  function Sign(x:Real) return Real is
  begin
    if x < 0.0 then
      return -1.0;
    elsif x > 0.0 then
      return 1.0;
    else
      return 0.0;
    end if;
  end Sign;

end Engine_3D.Math;
