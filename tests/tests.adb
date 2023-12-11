with Ada.Text_IO; use Ada.Text_IO;
with Test_Matrice;
with Test_Graphe;
with Test_Trifusion;

procedure Tests is
begin
  Put_Line ("Démarrage des tests");
  Test_Matrice;
  Test_Graphe;
  Test_Trifusion;
  Put_Line ("Fin des tests");
end Tests;
