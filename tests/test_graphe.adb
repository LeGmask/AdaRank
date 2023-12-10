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

  procedure Tester_Graphe is

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
      G        : T_Matrice (1 .. N, 1 .. N);
      Sortants : T_Matrice (1 .. N, 1 .. 1);
    begin
      Lire_Graphe (File, G, Sortants);
      Put_Line ("Graphe :");
      Afficher_Matrice (G);
      Put_Line ("Sortants :");
      Afficher_Matrice (Sortants);
      Ponderer_Graphe (G, Sortants);
      Put_Line ("Graphe pondéré :");
      Afficher_Matrice (G);
    end;
  end Tester_Graphe;

begin

  Tester_Graphe;

end Test_Graphe;
