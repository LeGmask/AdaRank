with Ada.Text_IO; use Ada.Text_IO;
with Test_Matrice;
with Test_Trifusion;
with Test_Export;

procedure Tests is
begin
  Put_Line ("Démarrage des tests");
  Test_Matrice;
  Test_Trifusion;
  Test_Export;
  Put_Line ("Fin des tests");
end Tests;
