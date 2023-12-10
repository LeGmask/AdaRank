--  with Ada.Text_IO; use Ada.Text_IO;
with Matrice;

procedure Test_Matrice is
  package T_Matrice_Int is new Matrice
   (T_Valeur => Integer, Neutre => 0, "+" => Standard."+",
    "*"      => Standard."*");
  use T_Matrice_Int;

  procedure Tester_Init is
    A : T_Matrice (1 .. 3, 1 .. 1);
    B : constant T_Matrice := ((1, 2), (3, 4), (5, 6));
  begin
    Init (A, 1);

    pragma Assert (A (1, 1) = 1 and A (2, 1) = 1 and A (3, 1) = 1);
    pragma Assert
     (B (1, 1) = 1 and B (1, 2) = 2 and B (2, 1) = 3 and B (2, 2) = 4 and
      B (3, 1) = 5 and B (3, 2) = 6);
    -- Afficher_Mat_Int(Mat);
  end Tester_Init;

  procedure Tester_Plus is
    A : constant T_Matrice := ((1, 2), (3, 4), (5, 6));
    B : constant T_Matrice := ((1, 2), (3, 4), (5, 6));
    C : T_Matrice (1 .. 3, 1 .. 2);
  begin
    C := A + B;
    pragma Assert
     (C (1, 1) = 2 and C (1, 2) = 4 and C (2, 1) = 6 and C (2, 2) = 8 and
      C (3, 1) = 10 and C (3, 2) = 12);
  end Tester_Plus;

  procedure Tester_Mult is
    A : constant T_Matrice := ((1, 2), (3, 4), (5, 6));
    B : constant T_Matrice := ((1, 2, 3), (4, 5, 6));
    C : T_Matrice (1 .. 3, 1 .. 3);
  begin
    C := A * B;
    pragma Assert
     (C (1, 1) = 9 and C (1, 2) = 12 and C (1, 3) = 15 and C (2, 1) = 19 and
      C (2, 2) = 26 and C (2, 3) = 33 and C (3, 1) = 29 and C (3, 2) = 40 and
      C (3, 3) = 51);

    C := C * 2;
    pragma Assert
     (C (1, 1) = 18 and C (1, 2) = 24 and C (1, 3) = 30 and C (2, 1) = 38 and
      C (2, 2) = 52 and C (2, 3) = 66 and C (3, 1) = 58 and C (3, 2) = 80 and
      C (3, 3) = 102);

    C := 2 * C;
    pragma Assert
     (C (1, 1) = 36 and C (1, 2) = 48 and C (1, 3) = 60 and C (2, 1) = 76 and
      C (2, 2) = 104 and C (2, 3) = 132 and C (3, 1) = 116 and
      C (3, 2) = 160 and C (3, 3) = 204);
  end Tester_Mult;

  procedure Tester_Transpose is
    A : constant T_Matrice := ((1, 2), (3, 4), (5, 6));
    B : T_Matrice (1 .. 2, 1 .. 3);
  begin
    B := Transpose (A);
    pragma Assert
     (B (1, 1) = 1 and B (1, 2) = 3 and B (1, 3) = 5 and B (2, 1) = 2 and
      B (2, 2) = 4 and B (2, 3) = 6);

  end Tester_Transpose;
begin
  Tester_Init;

  Tester_Plus;
  Tester_Mult;

  Tester_Transpose;
end Test_Matrice;
