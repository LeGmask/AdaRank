with Ada.Text_IO; use Ada.Text_IO;
with Matrice;

procedure Test_Matrice is
  package T_Matrice_Int is 
    new Matrice(T_Valeur => Integer, "+" => Standard."+", "*" => Standard."*");
  use T_Matrice_Int;

  procedure Afficher_Valeur(Valeur: Integer) is begin
    Put(Integer'Image(Valeur));
  end Afficher_Valeur;
  procedure Afficher_Mat_Int is new Afficher(Afficher_Valeur);

  -- Mat: T_Matrice := ((1), (1));
  Mat: T_Matrice(1..5, 1..1);

  A: T_Matrice := (
      (1, 2, 3),
      (4, 5, 6)
   );
  B: T_Matrice := (
      (1, 2),
      (3, 4),
      (5, 6)
   );
begin
  Init(Mat, 1);
  Afficher_Mat_Int(Mat + Mat);

  -- Put(Dim(M at));
  Afficher_Mat_Int(A*B);

  Afficher_Mat_Int(Transpose(A));


end Test_Matrice;
