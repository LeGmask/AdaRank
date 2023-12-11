with Trifusion;
with Matrice;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;


procedure Test_Trifusion is

    package Matrice_Float is new Matrice (Float, 0.0, "<", "+", "*");
    use Matrice_Float;

    procedure Put_Float (F : Float) is
    begin
        Put (F);
    end Put_Float;

    package Trifusion_Float is new Trifusion (Matrice_Float);
    use Trifusion_Float;    

    procedure Afficher_Matrice is new Afficher (Put_Float);

    A : T_Matrice := (1 => (10.0,7.0,4.0,8.0,1.0,2.0,6.0,9.0,3.0,5.0));
    Ordre : T_Matrice(1..1,1..A'Length(2));

begin
    for I in A'Range(2) loop
        Ordre(1,I) := Float (I);
    end loop;
    Tri(A,Ordre);
    -- Afficher_Matrice(Ordre);
    -- Afficher_Matrice(A);
    pragma Assert ( 0.00001 > abs (A(1,1)-10.0)
    and 0.00001 > abs (A(1,2)-9.0)
    and 0.00001 > abs (A(1,3)-8.0)
    and 0.00001 > abs (A(1,4)-7.0)
    and 0.00001 > abs (A(1,5)-6.0)
    and 0.00001 > abs (A(1,6)-5.0)
    and 0.00001 > abs (A(1,7)-4.0)
    and 0.00001 > abs (A(1,8)-3.0)
    and 0.00001 > abs (A(1,9)-2.0)
    and 0.00001 > abs (A(1,10)-1.0));
end Test_Trifusion;