with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Graphe;
with Matrice;

procedure Test_Graphe is

  package Matrice_Float is new Matrice (Float, 0.0, "+", "*");
  use Matrice_Float;
  package Graphe_Float is new Graphe (Matrice_Float, 1.0, "/");
  use Graphe_Float;

  procedure Tester_Graphe (Plein : in Boolean) is

    procedure Put_Float (F : in Float) is
    begin
      Put (F);
    end Put_Float;

    procedure Afficher_Matrice is new Afficher (Put_Float);
    File : File_Type;
    N    : Integer;
  begin
    Open (File, In_File, "networks/test.net");
    Get (File, N);

    declare
      G        : T_Matrice (N, N, Plein);
      Sortants : T_Matrice (N, 1, Plein);
    begin
      Lire_Graphe (File, G, Sortants);

      pragma Assert
       (Get (G, 1, 1) = 0.0 and Get (G, 1, 2) = 1.0 and Get (G, 1, 3) = 1.0 and
        Get (Sortants, 1, 1) = 2.0 and Get (G, 2, 1) = 0.0 and
        Get (G, 2, 2) = 0.0 and Get (G, 2, 3) = 1.0 and
        Get (Sortants, 2, 1) = 1.0 and Get (G, 3, 1) = 0.0 and
        Get (G, 3, 2) = 1.0 and Get (G, 3, 3) = 0.0 and
        Get (Sortants, 3, 1) = 1.0);

      Ponderer_Graphe (G, Sortants);

      pragma Assert
       (Get (G, 1, 1) = 0.0 and Get (G, 1, 2) = 0.5 and Get (G, 1, 3) = 0.5 and
        Get (G, 2, 1) = 0.0 and Get (G, 2, 2) = 0.0 and Get (G, 2, 3) = 1.0 and
        Get (G, 3, 1) = 0.0 and Get (G, 3, 2) = 1.0 and Get (G, 3, 3) = 0.0);

      Detruire (G);
      Detruire (Sortants);
    end;

    Close (File);
  end Tester_Graphe;

begin
  Put_Line (">>> Test du module Graphe plein...");
  Put ("  -> Test du mode plein... ");
  Tester_Graphe (True);
  Put_Line (" OK");
  Put ("  -> Test du mode creux... ");
  Tester_Graphe (False);
  Put_Line (" OK");
  Put_Line ("<<< Module Graphe OK");
end Test_Graphe;
