package body Engine_3D.Sorting is

  procedure Swap(a,b: in out Integer) is    pragma Inline(Swap);
    c: Integer; begin c:= a;a:= b;b:= c; end;

  procedure Sort_faces(o: in out object_3D) is
    -- Faces with the HIGHEST Z-val is placed last in Order
    -- (was descending before 1.IV.1999)
    last_face : constant Natural:= o.Num_of_faces;
    Center_Z  : Center_array renames o.Center_Z( 1 .. last_face );
    Order     : Index_array   renames o.Order;

    subtype Item is Integer;  -- z-values (sorting key)
    subtype Index is Integer; -- face numbers

    -- Adaptated from Swing's Ada class library
    -- URL: http://fedelma.astro.univie.ac.at/web/home.html
    -- Copyright (c) 1995 Swing Informationssysteme GmbH        All rights reserved
    -- tanzer(~)swing.co.at                 Glasauergasse 32, A--1130 Wien, Austria

    procedure Sort_Quick is

      type Side is (Left,right);
      type Sides is array (Side) of Index;
      Stack: array (1..32) of Sides;           -- Up To 2^32 Elements
      S: Integer := 1;
      I, J, L, R: Index;
      H: Item;

    begin
      Stack(S) := (1, last_face);

      while S > 0 loop
        L := Stack(S)(Left);
        R := Stack(S)(Right);
        S := S - 1;
        while L < R loop
          i := L;
          j := R;
          H := Center_Z((L+R)/2);
          loop
            loop
              I := Index'Succ(I);
              exit when Center_Z(I) >= H;
            end loop;
            loop
              J := Index'Pred(J);
              exit when H >= Center_Z(J);
            end loop;

            exit when J <= I;
            Swap(Center_Z(I), Center_Z(J));
            Swap(Order(I),    Order(J));
          end loop;

          if J-L < R-I then
            if i < R then                -- Stack(I, R)
              S := S + 1;
              Stack(S) := (I, R);
            end if;
            R := J;                      -- Proceed In(L, J)
          else
            if L < j then                -- Stack(L, J)
              S := S + 1;
              Stack(S) := (L, J);
            end if;
            L := I;                      -- Proceed In(I, R)
          end if;
        end loop;
      end loop;
    end Sort_Quick;

    procedure Sort_Straight is
      i: Index;
      h: Item; ho: Integer;

    begin
      for j in 2..last_face loop
        h  := Center_Z (j);
        ho := Order (j);
        i := j;
        while (i > 1) and then (h < Center_Z (Index'Pred (i))) loop
          Center_Z (i):= Center_Z (Index'Pred (i));
          Order (i)   := Order    (Index'Pred (i));
          i           := Index'Pred (i);
        end loop;
        Center_Z (i) := h;
        Order (i)   := ho;
      end loop;
    end Sort_Straight;

    procedure Sort_Quick_Straight is

    begin
      if Index'Pos(last_face) > 14 then
        Sort_Quick;
      else
        Sort_Straight;
      end if;
      --      Sort_Straight;
    end Sort_Quick_Straight;

    procedure Sort_Shell is
      T        : Integer := 3;
      I, K, IK : Index;
      Y        : Item;
      YO       : Integer;
      H   : constant array (1..12) of Integer :=
        (1, 4, 13, 40, 121, 364, 1093,
        3280, 9841, 28444, 85333, 256000);

    begin
      while T < H'Last and then Index'Val(H(T)) < last_face loop
        T := T + 1;
      end loop;
      T := T - 2;
      for S in reverse 1..T loop
        k := Index'Val(H (S));
        for j in Index'Succ(K)..last_face loop
          i := Index'Val(Index'Pos(J) - Index'Pos(K));
          Y  := Center_Z(J);
          YO := Order(J);
          while I >= 1 and then Y < Center_Z(I) loop
            IK:= Index'Val(Index'Pos(I)+Index'Pos(K));
            Center_Z(IK) := Center_Z(I);
            Order(IK)    := Order(I);
            i := Index'Val(Index'Pos(I) - Index'Pos(K));
          end loop;
          IK:= Index'Val(Index'Pos(I)+Index'Pos(K));
          Center_Z(IK) := Y;
          Order(IK)   := YO;
        end loop;
      end loop;
    end Sort_Shell;

    -- This procedure has been written in Ada by Jerome Delcourt
    -- from the idea of Karl-Dietrich Neubert
    -- Adapated to context (key+order)

    procedure FlashSort1(V : in out Center_array) is
      M : constant Index := V'Length / 10 + 1;
      K : Index := M;
      NMove : Index := 0;
      Hold, Flash, Swapp : Item;
      HoldO, FlashO, SwappO : Integer;
      J : Index := 1;
      IndexVMax : Index := V'First;
      VMin : Item := Item'Last;
      L : array(1..M) of Index := (others => 0);
      C1 : Float;
      THRESHOLD: constant:= 80;
    begin
      ---------------------
      -- CLASS FORMATION --
      ---------------------
      for I in V'Range loop
        if (V(I) > V(IndexVMax)) then
          IndexVMax := I;
        elsif (V(I) < VMin) then
          VMin := V(I);
        end if;
      end loop;
      -- If VMin = V(IndexVMax), V is already sorted...
      if (VMin = V(IndexVMax)) then
        return;
      end if;
      C1 := Float(M-1) / Float(V(IndexVMax) - VMin);
      for I in V'Range loop
        K := 1 + Index(C1 * Float(V(I) - VMin));
        L(K) := L(K) + 1;
      end loop;
      for K in 2..M loop
        L(K) := L(K) + L(K-1);
      end loop;
      --     Swap(V(1), V(IndexVMax));
      --     Swap(Order(1),    Order(IndexVMax));
      Swapp:=  V(1);  V(1):= V(IndexVMax); V(IndexVMax):= Swapp;
      SwappO:= Order(1); Order(1):= Order(IndexVMax);
      Order(IndexVMax):= SwappO;
      -----------------
      -- PERMUTATION --
      -----------------
      while (NMove < (V'Last - 1)) loop
        while (J > L(K)) loop
          J := J + 1;
          K := 1 + Index(C1 * Float(V(J) - VMin));
        end loop;
        Flash  := V(J);
        FlashO := Order(J);
        while (J /= L(K) + 1) loop
          K := 1 + Index(C1 * Float(Flash - VMin));
          --         Swap(Flash,  V(L(K)));
          --         Swap(FlashO, Order(L(K)));
          Swapp:=  Flash;  Flash:= V(L(K)); V(L(K)):= Swapp;
          SwappO:= FlashO; FlashO:= Order(L(K));   Order(L(K)):= SwappO;
          L(K) := L(K) - 1;
          NMove := NMove + 1;
        end loop;
      end loop;
      -----------------------------------------------
      -- Choice of RECURSION or STRAIGHT INSERTION --
      -----------------------------------------------
      for k in L'First .. L'Last-1 loop
        declare
          nx: constant Index:= L(k+1) - L(k);
        begin
          if False and nx > THRESHOLD then -- use recursion
            Flashsort1( V( L(k)+1 .. L(k)+nx ) );
          else                                  -- use insertion sort
            for I in reverse L(k)+1..L(k+1)-1 loop

              --          FOR I IN REVERSE V'FIRST..V'Last-2 LOOP
              if (V(I + 1) < V(I)) then
                Hold  := V(I);
                HoldO := Order(I);
                J := I;
                while (V(J + 1) < Hold) loop
                  V(J) := V(J+1);
                  Order(J)    := Order(J+1);
                  J := J+1;
                end loop;
                V(J) := Hold;
                Order(J)    := HoldO;
              end if;
            end loop;

          end if;
        end;
      end loop;
    end FlashSort1;

  begin              -- We can test all sorts of sorting...

    -- 5.IV.1999: o.Center_Z is filled using previous sorting -> no reset ->
    -- should speedup a new sorting for an object not rotating too fast!

    --    Sort_Straight;
    --    Sort_Quick;
    --    Sort_Quick_Straight;
    --    Sort_Shell;
    FlashSort1( Center_Z );
  end Sort_faces;

end Engine_3D.Sorting;
