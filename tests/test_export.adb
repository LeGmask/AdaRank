with Matrice;
with Export;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_export is

   package Matrice_Integer is new Matrice (Integer, 0, 1, "+", "*", "/");

   package Matrice_Float is new Matrice (Float, 0.0, 1.0, "+", "*", "/");

   package Export_PageRank is new Export
     (Matrice_Pi => Matrice_Float, Matrice_Ordre => Matrice_Integer);
   use Export_PageRank;

   procedure Put_Element (Fichier : File_Type; E : Float) is
   begin
      Put (Fichier, E, 1);
   end Put_Element;
   procedure Put_Element (Fichier : File_Type; E : Integer) is
   begin
      Put (Fichier, E, 1);
   end Put_Element;

   procedure Sauver is new Export_Resultats (Put_Element, Put_Element);

   procedure Tester_export(Plein: in Boolean) is
      Pi      : Matrice_Float.T_Matrice (1, 10, Plein);
      Ordre   : Matrice_Integer.T_Matrice (1, 10, Plein);
      N       : constant Integer := 10;
      Alpha   : constant Float   := 0.85;
      K       : constant Integer := 150;
      Prefixe : constant String  := "test";

      File     : File_Type;
      Entier   : Integer;
      Flottant : Float;
   begin
      Matrice_Float.Set (Pi, 1, 1, 10.0);
      Matrice_Float.Set (Pi, 1, 2, 7.0);
      Matrice_Float.Set (Pi, 1, 3, 4.0);
      Matrice_Float.Set (Pi, 1, 4, 8.0);
      Matrice_Float.Set (Pi, 1, 5, 1.0);
      Matrice_Float.Set (Pi, 1, 6, 2.0);
      Matrice_Float.Set (Pi, 1, 7, 6.0);
      Matrice_Float.Set (Pi, 1, 8, 9.0);
      Matrice_Float.Set (Pi, 1, 9, 3.0);
      Matrice_Float.Set (Pi, 1, 10, 5.0);

      Matrice_Integer.Set (Ordre, 1, 1, 1);
      Matrice_Integer.Set (Ordre, 1, 2, 8);
      Matrice_Integer.Set (Ordre, 1, 3, 4);
      Matrice_Integer.Set (Ordre, 1, 4, 2);
      Matrice_Integer.Set (Ordre, 1, 5, 7);
      Matrice_Integer.Set (Ordre, 1, 6, 10);
      Matrice_Integer.Set (Ordre, 1, 7, 3);
      Matrice_Integer.Set (Ordre, 1, 8, 9);
      Matrice_Integer.Set (Ordre, 1, 9, 6);
      Matrice_Integer.Set (Ordre, 1, 10, 5);

      Sauver (Pi, Ordre, N, K, Alpha, Prefixe);
      Open (File, In_File, Prefixe & ".prw");
      Get (File, Entier);
      pragma Assert (Entier = 10);
      Get (File, Flottant);
      pragma Assert (abs (Flottant - 0.85) < 0.000_1);
      Get (File, Entier);
      pragma Assert (Entier = 150);
      for I in 1 .. 10 loop
         Get (File, Flottant);
         pragma Assert
           (abs (Flottant - Matrice_Float.Get (Pi, 1, I)) < 0.000_1);
      end loop;
      Close (File);
      Open (File, In_File, Prefixe & ".pr");
      for I in 1 .. 10 loop
         Get (File, Entier);
         pragma Assert (Entier = Matrice_Integer.Get (Ordre, 1, I));
      end loop;
      Close (File);

      Matrice_Float.Detruire (Pi);
      Matrice_Integer.Detruire (Ordre);
   end Tester_export;

begin
   Put_Line (">>> Test du module d'Export...");
   Put("  -> Test du mode plein...");
   Tester_export (True);
   Put_Line (" OK");
   Put("  -> Test du mode creux...");
   Tester_export (False);
   Put_Line (" OK");
   Put_Line ("<<< Fin du test du module d'Export");
end Test_export;
