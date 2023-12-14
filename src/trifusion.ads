with Matrice;

generic
    with package Matrice_Tri is new Matrice (<>);
    use Matrice_Tri;
    with package Matrice_Ordre is new Matrice (<>);

    with function "<"
       (Gauche : in T_Valeur; Droite : in T_Valeur) return Boolean;
    -- Comparateur de T_Valeurs
    -- @param Gauche : la T_Valeur de gauche
    -- @param Droite : la T_Valeur de droite
package Trifusion is

    procedure Tri (Vecteur : in out T_Matrice; Ordre : out Matrice_Ordre.T_Matrice);
    -- Trie un vecteur ligne de T_Valeurs par ordre décroissant selon la méthode du trifusion et stocke dans "Ordre" les déplacements de valeurs effectués (Ordre doit avoir été initialisée avec des valeurs)
    -- @param Vecteur : le vecteur à trier
    -- @param Ordre : le vecteur qui stockera les déplacements de valeurs

end Trifusion;
