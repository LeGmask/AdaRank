with Ada.Text_IO; use Ada.Text_IO;
with Matrice;

generic
    with package Matrice_Pi is new Matrice (<>);
    use Matrice_Pi;
    with package Matrice_Ordre is new Matrice (<>);

package Export is 
    generic
        with procedure Put_Pi (File: File_Type; Valeur : Matrice_Pi.T_Valeur);
        with procedure Put_Ordre (File: File_Type; Valeur : Matrice_Ordre.T_Valeur);
    procedure Export_Resultats (Pi: in T_Matrice; Ordre: in Matrice_Ordre.T_Matrice; N, K: in Integer; Alpha: in Float; Prefix:  in String); 
end Export;