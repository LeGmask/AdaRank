with Ada.Text_IO; use Ada.Text_IO;

generic
  type T_Valeur is private; -- Type des valeurs de la matrice

  Zero : in T_Valeur; -- Zero pour l'addition
  Un : in T_Valeur;  -- Un pour l'initialisation

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
  with function "/" (A : T_Valeur; B : T_Valeur) return T_Valeur;
  -- Division entre deux T_Valeurs
  -- @param A : Numérateur
  -- @param B : Dénominateur
package Matrice is
  -- type T_Dim is array (1 .. 2) of Integer;
  type T_Matrice (Lignes, Colonnes : Positive; Pleine : Boolean) is private;

  procedure Init (Mat : out T_Matrice; Val : in T_Valeur := Zero);
  -- Initialise une matrice avec une valeur par défaut
  -- @param Mat : la matrice à initialiser
  -- @param Val : la valeur par défaut

  procedure Init_Fichier (File : in File_Type; Mat, Sortants : out T_Matrice);
  -- Crée une matrice à partir d'un fichier
  -- @param File : Fichier contenant la description du graphe
  -- @param Mat : Matrice d'adjacence à créer

  function Copie (Mat : in T_Matrice) return T_Matrice with
   Post =>
    Copie'Result.Lignes = Mat.Lignes and Copie'Result.Colonnes = Mat.Colonnes;
  -- Renvoie une copie d'une matrice
  -- @param Mat : la matrice à copier

  procedure Detruire (Mat : in out T_Matrice);
  -- Détruit une matrice
  -- @param Mat : la matrice à détruire

  function Get (Mat : in T_Matrice; Ligne, Colonne : Positive) return T_Valeur;
  -- Renvoie la valeur d'une matrice à une position donnée
  -- @param Matrice : la matrice
  -- @param Ligne : la ligne
  -- @param Colonne : la colonne

  procedure Set
   (Mat : in out T_Matrice; Ligne, Colonne : Positive; Val : in T_Valeur);
  -- Modifie la valeur d'une matrice à une position donnée
  -- @param Matrice : la matrice
  -- @param Ligne : la ligne
  -- @param Colonne : la colonne
  -- @param Val : la valeur

  -- function Dim(Mat: in T_Matrice) return T_Dim with
  -- Post => Dim(Mat)(0) = Mat'Length(0) and Dim(Mat)(1) = Mat'Length(1);

  function "+" (A, B : in T_Matrice) return T_Matrice with
   Pre =>
    A.Lignes = B.Lignes and A.Colonnes = B.Colonnes and A.Pleine = B.Pleine;
  -- Renvoie l'addition de deux matrices
  -- @param A : la première matrice
  -- @param B : la seconde matrice

  function "*" (A, B : in T_Matrice) return T_Matrice with
   Pre  => A.Colonnes = B.Lignes,
   Post => "*"'Result.Lignes = A.Lignes and "*"'Result.Colonnes = B.Colonnes;
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

  generic
    with procedure Traiter
     (Ligne, Colonne : in Integer; Valeur : in out T_Valeur);
  -- Procédure de traitement d'une T_Valeur
  -- @param Valeur : la valeur à traiter
  procedure Pour_Chaque (Mat : in T_Matrice);
  -- Applique une procédure à chaque valeur d'une matrice
  -- @param Mat : la matrice

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
private
  type T_Matrice_Pleine is
   array (Positive range <>, Positive range <>) of T_Valeur;
  -- Matrice pleine pour les opérations

  type T_Cellule;
  -- Cellule pour T_Vecteur_Creux

  type T_Vecteur_Creux is access T_Cellule;

  type T_Cellule is record
    Ligne    : Positive;
    Valeur   : T_Valeur;
    Suivante : T_Vecteur_Creux;
  end record;

  type T_Matrice_Creuse is array (Positive range <>) of T_Vecteur_Creux;
  -- Matrice creuse pour les opérations

  type T_Matrice (Lignes, Colonnes : Positive; Pleine : Boolean) is record
    case Pleine is
      when True =>
        Matrice_Pleine : T_Matrice_Pleine (1 .. Lignes, 1 .. Colonnes);
      when False =>
        Matrice_Creuse : T_Matrice_Creuse (1 .. Colonnes);
    end case;
  end record;
end Matrice;
