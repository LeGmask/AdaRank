with Ada.Text_IO; use Ada.Text_IO;
with Matrice;

procedure Test_Matrice is
  package T_Matrice_Int is 
    new Matrice(T_Valeur => Integer, "+" => Standard."+", "*" => Standard."*");
  use T_Matrice_Int;


  procedure Tester_Init is
    procedure Afficher_Valeur(Valeur: Integer) is begin
      Put(Integer'Image(Valeur));
    end Afficher_Valeur;
    procedure Afficher_Mat_Int is new Afficher(Afficher_Valeur);

    A: T_Matrice(1..3, 1..1);
    B: constant T_Matrice := ((1,2), (3,4), (5, 6));
  begin
    Init(A, 1);

    pragma Assert (A(1,1) = 1 and A(2,1) = 1 and A(3,1) = 1);
    pragma Assert (
      B(1,1) = 1 and B(1,2) = 2 and 
      B(2,1) = 3 and B(2,2) = 4 and
      B(3,1) = 5 and B(3,2) = 6
    );
    -- Afficher_Mat_Int(Mat);
  end Tester_Init;
begin
  Tester_Init;

end Test_Matrice;
