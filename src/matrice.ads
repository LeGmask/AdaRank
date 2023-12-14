generic
  type T_Valeur is private; -- Type des valeurs de la matrice

  Neutre : in T_Valeur; -- Neutre pour l'addition

  with function "+"
   (Gauche : in T_Valeur; Droite : in T_Valeur) return T_Valeur;
   -- Renvoie l'addition de deux T_Valeurs
   -- @param Gauche : la T_Valeur de gauche
   -- @param Droite : la T_Valeur de droite
  with function "*"
   (Gauche : in T_Valeur; Droite : in T_Valeur) return T_Valeur;
    -- Renvoie le produit de deux T_Valeurs
    -- @param Gauche : la T_Valeur de gauche
    -- @param Droite : la T_Valeur de droite
package Matrice is
  -- type T_Dim is array (1 .. 2) of Integer;
  type T_Matrice is array (Positive range <>, Positive range <>) of T_Valeur;

  procedure Init (Mat : out T_Matrice; Val : in T_Valeur := Neutre);
  -- Initialise une matrice avec une valeur par défaut
  -- @param Mat : la matrice à initialiser
  -- @param Val : la valeur par défaut

  -- function Dim(Mat: in T_Matrice) return T_Dim with
  -- Post => Dim(Mat)(0) = Mat'Length(0) and Dim(Mat)(1) = Mat'Length(1);

  function "+" (A, B : in T_Matrice) return T_Matrice with
   Pre => A'Length (1) = B'Length (1) and A'Length (2) = B'Length (2);
  -- Renvoie l'addition de deux matrices
  -- @param A : la première matrice
  -- @param B : la seconde matrice

  function "*" (A, B : in T_Matrice) return T_Matrice with
   Pre  => A'Length (2) = B'Length (1),
   Post =>
    "*"'Result'Length (1) = A'Length (1) and
    "*"'Result'Length (2) = B'Length (2);
  -- Renvoie le produit de deux matrices
  -- @param A : la première matrice
  -- @param B : la seconde matrice

  function "*" (A : in T_Matrice; B : in T_Valeur) return T_Matrice;
  -- Renvoie le produit d'une matrice par un scalaire à droite
  -- @param A : la matrice
  -- @param B : le scalaire
  function "*" (A : in T_Valeur; B : in T_Matrice) return T_Matrice;
  -- Renvoie le produit d'une matrice par un scalaire à gauche
  -- @param A : le scalaire
  -- @param B : la matrice

  function Transpose (A : in T_Matrice) return T_Matrice;
  -- Renvoie la transposée d'une matrice
  -- @param A : la matrice

  generic
    with procedure Afficher_Valeur (Valeur : T_Valeur);
    -- Procédure d'affichage d'une T_Valeur
    -- @param Valeur : la valeur à afficher
  procedure Afficher (Mat : in T_Matrice);
  -- Affiche une matrice à l'aide d'une procédure d'affichage de valeur à donner à l'instanciation
  -- @param Mat : la matrice à afficher

end Matrice;
