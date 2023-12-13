with Matrice;

generic
    with package Matrice_Tri is new Matrice (<>);
    use Matrice_Tri;
    with package Matrice_Ordre is new Matrice (<>);

    with function "<"
       (Gauche : in T_Valeur; Right : in T_Valeur) return Boolean;
package Trifusion is

    procedure Tri (Vecteur : in out T_Matrice; Ordre : out Matrice_Ordre.T_Matrice);

end Trifusion;
