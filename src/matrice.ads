generic
  type T_Valeur is private;

  Neutre : in T_Valeur;

  with function "+"
   (Gauche : in T_Valeur; Right : in T_Valeur) return T_Valeur;
  with function "*"
   (Gauche : in T_Valeur; Right : in T_Valeur) return T_Valeur;
package Matrice is
  type T_Dim is array (1 .. 2) of Integer;
  type T_Matrice is array (Positive range <>, Positive range <>) of T_Valeur;

  procedure Init (Mat : out T_Matrice; Val : in T_Valeur := Neutre);

  -- function Dim(Mat: in T_Matrice) return T_Dim with
  -- Post => Dim(Mat)(0) = Mat'Length(0) and Dim(Mat)(1) = Mat'Length(1);

  function "+" (A, B : in T_Matrice) return T_Matrice with
   Pre => A'Length (1) = B'Length (1) and A'Length (2) = B'Length (2);

  function "*" (A, B : in T_Matrice) return T_Matrice with
   Pre  => A'Length (2) = B'Length (1),
   Post =>
    "*"'Result'Length (1) = A'Length (1) and
    "*"'Result'Length (2) = B'Length (2);

  function "*" (A : in T_Matrice; B : in T_Valeur) return T_Matrice;
  function "*" (A : in T_Valeur; B : in T_Matrice) return T_Matrice;

  function Transpose (A : in T_Matrice) return T_Matrice;

  generic
    with procedure Afficher_Valeur (Valeur : T_Valeur);
  procedure Afficher (Mat : in T_Matrice);

end Matrice;
