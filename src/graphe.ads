with Ada.Text_IO; use Ada.Text_IO;
with Matrice;

generic
    with package Matrice_Graphe is new Matrice (<>);
    use Matrice_Graphe;

    Defaut_Poid : T_Valeur; -- coefficient correspondant à un arc dans la matrice d'adjacence

    with function "/" (A : T_Valeur; B : T_Valeur) return T_Valeur;
    -- Division entre deux T_Valeurs
    -- @param A : Numérateur
    -- @param B : Dénominateur
package Graphe is

    --  package Matrice_Integer is new Matrice (Integer, "+", "*");
    --  use Matrice_Integer;

    procedure Lire_Graphe (File : in File_Type; Adj, Sortants : out T_Matrice);
    -- Calcule la matrice d'adjacence ainsi que le vecteur colonne des degrés sortants
    -- @param File : Fichier contenant la description du graphe
    -- @param Adj : Matrice d'adjacence
    -- @param Sortants : Vecteur colonne des degrés sortants
    procedure Ponderer_Graphe
       (Adj : in out T_Matrice; Sortants : in T_Matrice);
    -- Pondère la matrice d'adjacence selon les dégrés sortants
    -- @param Adj : Matrice d'adjacence (qui est pondérée)
    -- @param Sortants : Vecteur colonne des degrés sortants
end Graphe;
