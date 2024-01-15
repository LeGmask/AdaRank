with Trifusion;
with Matrice;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Trifusion is

    package Matrice_Integer is new Matrice (Integer, 0, 1, "+", "-", "*", "/");
    package Matrice_Float is new Matrice (Float, 0.0, 1.0, "+", "-", "*", "/");

    package Trifusion_Float is new Trifusion
       (Matrice_Float, Matrice_Integer, "<");
    use Trifusion_Float;

    procedure Tester_Trifusion(Plein: in Boolean) is
        A     : Matrice_Float.T_Matrice (1, 10, Plein);
        Ordre : Matrice_Integer.T_Matrice (1, A.Colonnes, Plein);
    begin
        Matrice_Float.Set (A, 1, 1, 10.0);
        Matrice_Float.Set (A, 1, 2, 7.0);
        Matrice_Float.Set (A, 1, 3, 4.0);
        Matrice_Float.Set (A, 1, 4, 8.0);
        Matrice_Float.Set (A, 1, 5, 1.0);
        Matrice_Float.Set (A, 1, 6, 2.0);
        Matrice_Float.Set (A, 1, 7, 6.0);
        Matrice_Float.Set (A, 1, 8, 9.0);
        Matrice_Float.Set (A, 1, 9, 3.0);
        Matrice_Float.Set (A, 1, 10, 5.0);

        for I in 1 .. A.Colonnes loop
            Matrice_Integer.Set (Ordre, 1, I, I);
        end loop;
        Tri (A, Ordre);

        pragma Assert
           (0.000_01 > abs (Matrice_Float.Get (A, 1, 1) - 10.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 2) - 9.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 3) - 8.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 4) - 7.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 5) - 6.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 6) - 5.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 7) - 4.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 8) - 3.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 9) - 2.0) and
            0.000_01 > abs (Matrice_Float.Get (A, 1, 10) - 1.0));

        pragma Assert
           (Matrice_Integer.Get (Ordre, 1, 1) = 1 and
            Matrice_Integer.Get (Ordre, 1, 2) = 8 and
            Matrice_Integer.Get (Ordre, 1, 3) = 4 and
            Matrice_Integer.Get (Ordre, 1, 4) = 2 and
            Matrice_Integer.Get (Ordre, 1, 5) = 7 and
            Matrice_Integer.Get (Ordre, 1, 6) = 10 and
            Matrice_Integer.Get (Ordre, 1, 7) = 3 and
            Matrice_Integer.Get (Ordre, 1, 8) = 9 and
            Matrice_Integer.Get (Ordre, 1, 9) = 6 and
            Matrice_Integer.Get (Ordre, 1, 10) = 5);

        Matrice_Float.Detruire (A);
        Matrice_Integer.Detruire (Ordre);
    end Tester_Trifusion;
begin
    Put_Line (">>> Test du module Trifusion...");
    Put ("  -> Test du mode plein... ");
    Tester_Trifusion(True);
    Put_Line(" OK");
    --  Put ("  -> Test du mode creux... ");
    --  Tester_Trifusion(False);
    --  Put_Line(" OK");
    Put_Line("<<< Test du module Trifusion OK");
end Test_Trifusion;
