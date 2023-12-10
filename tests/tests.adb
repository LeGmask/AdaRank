with Ada.Text_IO; use Ada.Text_IO;
with Test_Matrice;
with Test_Graphe;

procedure Tests is
begin
  Put_Line ("Démarage des tests");
  Test_Matrice;
  Test_Graphe;
  Put_Line ("Fin des tests");
end Tests;
