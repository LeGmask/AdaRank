with Matrice;

generic
    with package Matrice_Tri is new Matrice (<>);
    use Matrice_Tri;

package Trifusion is

    procedure Tri(Vecteur: in out T_Matrice; Ordre: in out T_Matrice);
    
end Trifusion;
