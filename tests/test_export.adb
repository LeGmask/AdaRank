with Matrice;
with Export;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_export is

    package Matrice_Integer is new Matrice (Integer, 0, "+", "*");
    
    package Matrice_Float is new Matrice (Float, 0.0, "+", "*");

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

    Pi      : constant Matrice_Float.T_Matrice    :=
       (1 => (10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0));
    Ordre   : constant Matrice_Integer.T_Matrice  :=
       (1 => (1, 8, 4, 2, 7, 10, 3, 9, 6, 5));
    N       : constant Integer                    := 10;
    Alpha   : constant Float                      := 0.85;
    K       : constant Integer                    := 150;
    Prefixe : constant String                     := "test";

    File    : File_Type;
    Entier  : Integer;
    Flottant: Float;
begin
    Sauver (Pi, Ordre, N, K, Alpha, Prefixe);
    Open(File, In_File, Prefixe & ".prw");
    Get(File, Entier);
    pragma Assert (Entier = 10);
    Get(File, Flottant);
    pragma Assert (abs (Flottant - 0.85) < 0.0001);
    Get(File, Entier);
    pragma Assert (Entier = 150);
    for I in 1 .. 10 loop
       Get(File, Flottant);
       pragma Assert (abs (Flottant - Pi(1, I)) < 0.0001);
    end loop;
    Close(File);
    Open(File, In_File, Prefixe & ".pr");
    for I in 1 .. 10 loop
       Get(File, Entier);
       pragma Assert (Entier = Ordre(1, I));
    end loop;
    Close(File);

end Test_Export;
