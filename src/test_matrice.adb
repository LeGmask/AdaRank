with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Matrice;

procedure Test_Matrice is
    package T_Matrice_Int is new Matrice
       (T_Valeur => Integer, Zero => 0, "+" => Standard."+",
        "*"      => Standard."*");
    use T_Matrice_Int;

    procedure Put_Int (F : in Integer) is
    begin
        Put (F);
    end Put_Int;

    procedure Afficher_Matrice is new Afficher (Put_Int);

    --  A : T_Matrice (Lignes => 3, Colonnes => 3, Pleine => False);
    B : T_Matrice (Lignes => 3, Colonnes => 2, Pleine => False);
    C : T_Matrice (Lignes => 2, Colonnes => 3, Pleine => False);
    D : T_Matrice (Lignes => 3, Colonnes => 3, Pleine => False);
    --  E : T_Matrice (Lignes => 3, Colonnes => 1, Pleine => False);

    --  A : T_Matrice (3, 2, True);
    --  B : T_Matrice (2, 3, True);
    --  C : T_Matrice (3, 3, True);
    
begin
    --  Set (A, 1, 1, 1);
    --  Set (A, 1, 2, 2);
    --  Set (A, 2, 1, 3);
    --  Set (A, 2, 2, 4);
    --  Set (A, 3, 1, 5);
    --  Set (A, 3, 2, 6);

    --  Set (B, 1, 1, 1);
    --  Set (B, 1, 2, 2);
    --  Set (B, 1, 3, 3);
    --  Set (B, 2, 1, 4);
    --  Set (B, 2, 2, 5);
    --  Set (B, 2, 3, 6);

    --  Afficher_Matrice(A);
    --  New_Line;
    --  Afficher_Matrice(B);
    --  New_Line;

    --  C := A * B;
    --  Afficher_Matrice (C);
    --  pragma Assert
    --   (Get (C, 1, 1) = 9 and Get (C, 1, 2) = 12 and Get (C, 1, 3) = 15 and
    --    Get (C, 2, 1) = 19 and Get (C, 2, 2) = 26 and Get (C, 2, 3) = 33 and
    --    Get (C, 3, 1) = 29 and Get (C, 3, 2) = 40 and Get (C, 3, 3) = 51);

    --  C := 2 * C;
    --  New_Line;
    --  Afficher_Matrice (C);
    --  pragma Assert
    --   (Get (C, 1, 1) = 18 and Get (C, 1, 2) = 24 and Get (C, 1, 3) = 30 and
    --    Get (C, 2, 1) = 38 and Get (C, 2, 2) = 52 and Get (C, 2, 3) = 66 and
    --    Get (C, 3, 1) = 58 and Get (C, 3, 2) = 80 and Get (C, 3, 3) = 102);
    --  Init (A, 3);

    --  --  Put_Int(3/3);

    --  --  4 - nb.lignes * nb.colonnes;
    --  Afficher_Matrice (A);

    --  New_Line;

    --  Set (A, 1, 1, 1);
    --  Set (A, 3, 3, 9);
    --  Set (A, 2, 2, 5);
    --  Set (A, 1, 2, 2);
    --  Set (A, 2, 1, 4);
    --  Set (A, 1, 3, 3);
    --  Set (A, 3, 1, 7);
    --  Set (A, 3, 2, 8);
    --  Set (A, 2, 3, 6);

    --  Set (A, 2, 2, 0);

    --  pragma Assert
    --     (Get (A, 1, 1) = 1 and Get (A, 1, 2) = 2 and Get (A, 1, 3) = 3 and
    --      Get (A, 2, 1) = 4 and Get (A, 2, 2) = 0 and Get (A, 2, 3) = 6 and
    --      Get (A, 3, 1) = 7 and Get (A, 3, 2) = 8 and Get (A, 3, 3) = 9);

    --  Afficher_Matrice (A);

    --  Set (A, 2, 1, 5);
    --  Set (A, 1, 2, 5);
    --  Set (A, 1, 3, 5);
    --  set (A, 2, 3, 5);

    --  Detruire (A);

    init (B, 3);

    Set (B, 1, 1, 1);
    C := Transpose (B);
    --  Afficher_Matrice (B);
    --  New_Line;
    --  Afficher_Matrice (C);
    --  New_Line;
    --  Set (D, 1, 1, 1);
    --  Set (D, 1, 3, 2);

    --  Afficher_Matrice (D);
    --  New_Line;
    --  D := 2 * D * 2;
    --  Afficher_Matrice (D);

    --  New_Line;


    --  Set (E, 1, 1, 1);
    --  Set (E, 2, 1, 1);
    --  Set (E, 3, 1, -8);>
    --  Afficher_Matrice(B*C);
    D := C * B;
    Detruire(B);
    Detruire(C);
    Detruire(D);

    --  Afficher_Matrice (D * E);

end Test_Matrice;
