with Ada.Text_IO; use Ada.Text_IO;
with Matrice;

generic
    with package Matrice_Graphe is new Matrice (<>);
    use Matrice_Graphe;

    Defaut_Poid : T_Valeur;

    with function "/" (A : T_Valeur; B : T_Valeur) return T_Valeur;
package Graphe is

    --  package Matrice_Integer is new Matrice (Integer, "+", "*");
    --  use Matrice_Integer;

    procedure Lire_Graphe (File : in File_Type; Adj, Sortants : out T_Matrice);
    procedure Ponderer_Graphe
       (Adj : in out T_Matrice; Sortants : in T_Matrice);
end Graphe;
