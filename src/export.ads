with Ada.Text_IO;         use Ada.Text_IO;
with Matrice;

generic
    with package Matrice_Pi is new Matrice (<>);
    use Matrice_Pi;
    with package Matrice_Ordre is new Matrice (<>);

package Export is
    generic
        with procedure Put_Pi (File : File_Type; Valeur : Matrice_Pi.T_Valeur);
        -- Affichage des T_Valeurs correspondant au type Matrice_Pi.T_Valeur dans le fichier File
        -- @param File : fichier dans lequel écrit la procédure
        -- @param Valeur : valeur à écrire dans le fichier
        with procedure Put_Ordre
           (File : File_Type; Valeur : Matrice_Ordre.T_Valeur);
    -- Affichage des T_Valeurs correspondant au type Matrice_Ordre.T_Valeur dans le fichier File
    -- @param File : fichier dans lequel écrit la procédure
    -- @param Valeur : valeur à écrire dans le fichier
    procedure Export_Resultats
       (Pi   : in T_Matrice; Ordre : in Matrice_Ordre.T_Matrice;
        N, K : in Integer; Alpha : in Float; Prefix : in String);
    -- Exporte les résultats de l'algorithme PageRank (Pi, Ordre (et N, K et Alpha)) dans les fichiers .pr et .prw dans le dossier courant, avec le préfixe Prefix
    -- @param Pi : Vecteur ligne des poids (trié)
    -- @param Ordre : Vecteur ligne des noeuds correspondants aux poids de Pi
    -- @param N : Nombre de noeuds
    -- @param K : Nombre maximal d'itérations effectuées par l'algorithme
    -- @param Alpha : Dumping factor
end Export;
