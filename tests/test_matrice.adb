with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Matrice;

procedure Test_Matrice is
  package T_Matrice_Int is new Matrice
   (T_Valeur => Integer, Zero => 0, Un => 1, "+" => Standard."+",
    "*"      => Standard."*", "/" => Standard."/");
  use T_Matrice_Int;

  procedure Tester_Init (Pleine : in Boolean) is
    A : T_Matrice (3, 3, Pleine);
    B : T_Matrice (3, 2, Pleine);
  begin
    Init (A, 1);

    Set (B, 1, 1, 1);
    Set (B, 1, 2, 2);
    Set (B, 2, 1, 4_578);
    Set (B, 2, 1, 0); -- creux
    Set (B, 2, 2, 0); -- creux
    Set (B, 3, 1, 5);
    Set (B, 3, 2, 6);

    pragma Assert
     (Get (A, 1, 1) = 1 and Get (A, 1, 2) = 1 and Get (A, 1, 3) = 1 and
      Get (A, 2, 1) = 1 and Get (A, 2, 2) = 1 and Get (A, 2, 3) = 1 and
      Get (A, 3, 1) = 1 and Get (A, 3, 2) = 1 and Get (A, 3, 3) = 1);
    pragma Assert
     (Get (B, 1, 1) = 1 and Get (B, 1, 2) = 2 and Get (B, 2, 1) = 0 and
      Get (B, 2, 2) = 0 and Get (B, 3, 1) = 5 and Get (B, 3, 2) = 6);

    Detruire (A);
    Detruire (B);
  end Tester_Init;

  procedure Tester_Init_Fichier (Pleine : in Boolean) is
    File : File_Type;
    N    : Integer;

    package T_Matrice_Float is new Matrice
     (T_Valeur => Float, Zero => 0.0, Un => 1.0, "+" => Standard."+",
      "*"      => Standard."*", "/" => Standard."/");
  begin
    Open (File, In_File, "networks/test.net");
    Get (File, N);

    declare
      Mat      : T_Matrice_Float.T_Matrice (N, N, Pleine);
      Sortants : T_Matrice_Float.T_Matrice (N, 1, Pleine);
    begin
      T_Matrice_Float.Init_Fichier (File, Mat, Sortants);

      if Pleine then
        pragma Assert
         (T_Matrice_Float.Get (Sortants, 1, 1) = 2.0 and
          T_Matrice_Float.Get (Sortants, 2, 1) = 1.0 and
          T_Matrice_Float.Get (Sortants, 3, 1) = 3.0);
      end if;
      
      pragma Assert
       (T_Matrice_Float.Get (Mat, 1, 1) = 0.0 and
        T_Matrice_Float.Get (Mat, 1, 2) = 0.5 and
        T_Matrice_Float.Get (Mat, 1, 3) = 0.5 and
        T_Matrice_Float.Get (Mat, 2, 1) = 0.0 and
        T_Matrice_Float.Get (Mat, 2, 2) = 0.0 and
        T_Matrice_Float.Get (Mat, 2, 3) = 1.0 and
        T_Matrice_Float.Get (Mat, 3, 1) = 1.0/3.0 and
        T_Matrice_Float.Get (Mat, 3, 2) = 1.0/3.0 and
        T_Matrice_Float.Get (Mat, 3, 3) = 1.0/3.0);

      T_Matrice_Float.Detruire (Mat);
    end;

    Close (File);
  end Tester_Init_Fichier;

  procedure Tester_Copie (Pleine : in Boolean) is
    A : T_Matrice (3, 3, Pleine);
    B : T_Matrice (3, 3, Pleine);
  begin

    Set (A, 1, 1, 1);
    Set (A, 1, 2, 2);
    Set (A, 1, 3, 3);
    Set (A, 2, 1, 4);
    --  Set (A, 2, 2, 5); -- creux
    Set (A, 2, 3, 6);
    Set (A, 3, 1, 7);
    Set (A, 3, 2, 8);
    Set (A, 3, 3, 9);

    B := Copie (A);
    for I in 1 .. B.Lignes loop
      for J in 1 .. B.Colonnes loop
        pragma Assert (Get (B, I, J) = Get (A, I, J));
      end loop;
    end loop;

    Detruire (A);
    Detruire (B);
  end Tester_Copie;

  procedure Tester_Plus (Pleine : in Boolean) is
    A : T_Matrice (3, 2, Pleine);
    B : T_Matrice (3, 2, Pleine);
    C : T_Matrice (3, 2, Pleine);
  begin
    Set (A, 1, 1, 1);
    Set (A, 1, 2, 2);
    Set (A, 2, 1, 3);
    Set (A, 2, 2, 4);
    Set (A, 3, 1, 5);
    Set (A, 3, 2, 6);

    Set (B, 1, 1, 1);
    Set (B, 1, 2, 2);
    Set (B, 2, 1, 3);
    Set (B, 2, 2, 4);
    Set (B, 3, 1, 5);
    Set (B, 3, 2, 6);

    C := A + B;
    pragma Assert
     (Get (C, 1, 1) = 2 and Get (C, 1, 2) = 4 and Get (C, 2, 1) = 6 and
      Get (C, 2, 2) = 8 and Get (C, 3, 1) = 10 and Get (C, 3, 2) = 12);

    Detruire (A);
    Detruire (B);
    Detruire (C);
  end Tester_Plus;

  procedure Tester_Mult (Pleine : in Boolean) is
    A : T_Matrice (3, 2, Pleine);
    B : T_Matrice (2, 3, Pleine);
    C : T_Matrice (3, 3, Pleine);
    D : T_Matrice (3, 3, Pleine);
  begin
    Set (A, 1, 1, 1);
    Set (A, 1, 2, 2);
    Set (A, 2, 1, 3);
    Set (A, 2, 2, 4);
    Set (A, 3, 1, 5);
    Set (A, 3, 2, 6);

    Set (B, 1, 1, 1);
    Set (B, 1, 2, 2);
    Set (B, 1, 3, 3);
    Set (B, 2, 1, 4);
    Set (B, 2, 2, 5);
    Set (B, 2, 3, 6);

    C := A * B;
    pragma Assert
     (Get (C, 1, 1) = 9 and Get (C, 1, 2) = 12 and Get (C, 1, 3) = 15 and
      Get (C, 2, 1) = 19 and Get (C, 2, 2) = 26 and Get (C, 2, 3) = 33 and
      Get (C, 3, 1) = 29 and Get (C, 3, 2) = 40 and Get (C, 3, 3) = 51);

    D := C * 2;
    pragma Assert
     (Get (D, 1, 1) = 18 and Get (D, 1, 2) = 24 and Get (D, 1, 3) = 30 and
      Get (D, 2, 1) = 38 and Get (D, 2, 2) = 52 and Get (D, 2, 3) = 66 and
      Get (D, 3, 1) = 58 and Get (D, 3, 2) = 80 and Get (D, 3, 3) = 102);
    Detruire (D);

    D := 2 * C;
    pragma Assert
     (Get (D, 1, 1) = 18 and Get (D, 1, 2) = 24 and Get (D, 1, 3) = 30 and
      Get (D, 2, 1) = 38 and Get (D, 2, 2) = 52 and Get (D, 2, 3) = 66 and
      Get (D, 3, 1) = 58 and Get (D, 3, 2) = 80 and Get (D, 3, 3) = 102);

    Detruire (A);
    Detruire (B);
    Detruire (C);
    Detruire (D);
  end Tester_Mult;

  procedure Tester_Mult_Mixte is
    A : T_Matrice (3, 3, True);
    B : T_Matrice (3, 3, False);
    C : T_Matrice (3, 3, True);
  begin
    Set (A, 1, 1, 1);
    Set (A, 1, 2, 2);
    Set (A, 1, 3, 3);
    Set (A, 2, 1, 4);
    Set (A, 2, 2, 5);
    Set (A, 2, 3, 6);
    Set (A, 3, 1, 7);
    Set (A, 3, 2, 8);
    Set (A, 3, 3, 9);

    Set (B, 1, 1, 1);
    --  Set (B, 1, 2, 2);
    Set (B, 1, 3, 3);
    Set (B, 2, 1, 4);
    --  Set (B, 2, 2, 5);
    --  Set (B, 2, 3, 6);
    Set (B, 3, 1, 7);
    --  Set (B, 3, 2, 8);
    Set (B, 3, 3, 9);

    C := A * B;
    pragma Assert
     (Get (C, 1, 1) = 30 and Get (C, 1, 2) = 0 and Get (C, 1, 3) = 30 and
      Get (C, 2, 1) = 66 and Get (C, 2, 2) = 0 and Get (C, 2, 3) = 66 and
      Get (C, 3, 1) = 102 and Get (C, 3, 2) = 0 and Get (C, 3, 3) = 102);
    Detruire (C);

    C := B * A;
    pragma Assert
     (Get (C, 1, 1) = 22 and Get (C, 1, 2) = 26 and Get (C, 1, 3) = 30 and
      Get (C, 2, 1) = 4 and Get (C, 2, 2) = 8 and Get (C, 2, 3) = 12 and
      Get (C, 3, 1) = 70 and Get (C, 3, 2) = 86 and Get (C, 3, 3) = 102);

    Detruire (C);
    Detruire (A);
    Detruire (B);
  end Tester_Mult_Mixte;

  procedure Tester_Transpose (Pleine : in Boolean) is
    A : T_Matrice (3, 2, Pleine);
    B : T_Matrice (2, 3, Pleine);
  begin
    Set (A, 1, 1, 1);
    Set (A, 1, 2, 2);
    Set (A, 2, 1, 3);
    Set (A, 2, 2, 4);
    Set (A, 3, 1, 5);
    Set (A, 3, 2, 6);

    B := Transpose (A);
    pragma Assert
     (Get (B, 1, 1) = 1 and Get (B, 1, 2) = 3 and Get (B, 1, 3) = 5 and
      Get (B, 2, 1) = 2 and Get (B, 2, 2) = 4 and Get (B, 2, 3) = 6);

    Detruire (A);
    Detruire (B);
  end Tester_Transpose;
begin
  Put_Line (">>> Test du module Matrice...");
  Put_Line (" - Mode creux :");

  Put ("  -> Test de l'initialisation... ");
  Tester_Init (False);
  Put_Line ("OK");

  Put ("  -> Test de l'initialisation par fichier... ");
  Tester_Init_Fichier (False);
  Put_Line ("OK");

  Put ("  -> Test de la copie... ");
  Tester_Copie (False);
  Put_Line ("OK");

  Put ("  -> Test de l'addition... ");
  Tester_Plus (False);
  Put_Line ("OK");

  Put ("  -> Test de la multiplication... ");
  Tester_Mult (False);
  Put_Line ("OK");

  Put ("  -> Test de la transposée... ");
  Tester_Transpose (False);
  Put_Line ("OK");

  Put_Line (" - Mode plein :");

  Put ("  -> Test de l'initialisation... ");
  Tester_Init (True);
  Put_Line ("OK");

  Put ("  -> Test de l'initialisation par fichier... ");
  Tester_Init_Fichier (True);
  Put_Line ("OK");

  Put ("  -> Test de la copie... ");
  Tester_Copie (True);
  Put_Line ("OK");

  Put ("  -> Test de l'addition... ");
  Tester_Plus (True);
  Put_Line ("OK");

  Put ("  -> Test de la multiplication... ");
  Tester_Mult (True);
  Put_Line ("OK");

  Put ("  -> Test de la transposée... ");
  Tester_Transpose (True);
  Put_Line ("OK");

  Put_Line (" - Mode mixte :");
  Put ("  -> Test de la multiplication... ");
  Tester_Mult_Mixte;
  Put_Line ("OK");
  Put_Line ("<<< Module Matrice OK");
end Test_Matrice;
