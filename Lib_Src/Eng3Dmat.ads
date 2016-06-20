------------------------------------------------------------------------------
--  File:            Eng3DMat.ads
--  Description:     Basic 3D engine : math, matrices, operators
--  Date / Version:  26-Mar-2001 ; 15-Oct-2000 ; 6-Aug-2000
------------------------------------------------------------------------------

package Engine_3D.Math is

  -- The fixed-point is home-made, so are the floor and ceiling functions
  function Floor(f: Fix) return Integer;
  function Ceiling(f: Fix) return Integer;

  pragma Inline(Floor,Ceiling);

  -- Vectors & matrices

  function To_RealPoint(p: Point) return RealPoint;
  function To_RealPoint(p: Fixed_Point) return RealPoint;
  function To_point(p: RealPoint) return Point;
  function To_fixed_point(p: RealPoint) return Fixed_Point;

  function To_Vector3(v: Fixed_vectT) return Vector3;

  function "*"(l:Real; v:Vector3) return Vector3;
  function "+"(a,b: Vector3) return Vector3;
  function "-"(a,b: Vector3) return Vector3;
  function "-"(a: Vector3) return Vector3;

  function "*"(a,b: Vector3) return Real;     -- dot product
  function Special_dot( N1,V2: Vector3; effect: Reflexion_type ) return Real;
    -- pragma Inline(Special_dot);

  function "*"(a,b: Vector3) return Vector3;  -- cross product
  function Norm(a: Vector3) return Real;
  function Norm2(a: Vector3) return Real;

  function "+"(a,b: Fixed_vectT) return Fixed_vectT;

  function To_Real(FM: FixedMatrix33) return Matrix33;
  function To_Fix(RM: Matrix33) return FixedMatrix33;

  function "*"(A,B: FixedMatrix33) return FixedMatrix33;
  function "*"(A,B: Matrix33) return Matrix33;
  function "*"(A: Matrix33; x: Vector3) return Vector3;

  function Transpose(A:Matrix33) return Matrix33;
  procedure Orthonormalize(M: in out Matrix33);

  -- Rem: the rotation matrices are orthogonal; for them, Inverse=Transpose
  function Det(A: Matrix33) return Real;
  function Inverse(A: Matrix33) return Matrix33;

  function XYZ_rotation(ax,ay,az: Real) return FixedMatrix33;
  function XYZ_rotation(ax,ay,az: Real) return Matrix33;
  -- A matrix is calculated once for each object and once for
  -- the whole view so we can avoid using sin/cos tables

  function Sign(x: Real) return Real;
  pragma Inline(Sign);

end Engine_3D.Math;
