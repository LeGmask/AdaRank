with Trifusion;
with Matrice;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_Trifusion is

    package Matrice_Integer is new Matrice (Integer, 0, "+", "*");
    --  use Matrice_Integer;
    package Matrice_Float is new Matrice (Float, 0.0, "+", "*");
    --  use Matrice_Float;

    --  procedure Put_Element (E : Float) is
    --  begin
        --  Put (E);
    --  end Put_Element;
    --  procedure Put_Element (E : Integer) is
    --  begin
    --      Put (E);
    --  end Put_Element;

    --  procedure Afficher_Matrice is new Matrice_Float.Afficher (Put_Element);
    --  procedure Afficher_Matrice is new Matrice_Integer.Afficher (Put_Element);

    package Trifusion_Float is new Trifusion
       (Matrice_Float, Matrice_Integer, "<");
    use Trifusion_Float;

    A     : Matrice_Float.T_Matrice :=
       (1 => (10.0, 7.0, 4.0, 8.0, 1.0, 2.0, 6.0, 9.0, 3.0, 5.0));
    Ordre : Matrice_Integer.T_Matrice (1 .. 1, 1 .. A'Length (2));

begin
    for I in A'Range (2) loop
        Ordre (1, I) := I;
    end loop;
    Tri (A, Ordre);
    --  Afficher_Matrice (Ordre);
    -- Afficher_Matrice(A);
    pragma Assert
       (0.000_01 > abs (A (1, 1) - 10.0) and
        0.000_01 > abs (A (1, 2) - 9.0) and 0.000_01 > abs (A (1, 3) - 8.0) and
        0.000_01 > abs (A (1, 4) - 7.0) and 0.000_01 > abs (A (1, 5) - 6.0) and
        0.000_01 > abs (A (1, 6) - 5.0) and 0.000_01 > abs (A (1, 7) - 4.0) and
        0.000_01 > abs (A (1, 8) - 3.0) and 0.000_01 > abs (A (1, 9) - 2.0) and
        0.000_01 > abs (A (1, 10) - 1.0));

    pragma Assert
       (Ordre (1, 1) = 1 and Ordre (1, 2) = 8 and Ordre (1, 3) = 4 and
        Ordre (1, 4) = 2 and Ordre (1, 5) = 7 and Ordre (1, 6) = 10 and
        Ordre (1, 7) = 3 and Ordre (1, 8) = 9 and Ordre (1, 9) = 6 and
        Ordre (1, 10) = 5);
end Test_Trifusion;
