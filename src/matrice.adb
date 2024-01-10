--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body Matrice is

  procedure Free is new Ada.Unchecked_Deallocation
   (Object => T_Cellule, Name => T_Vecteur_Creux);

  function Copie_Vect_Creux (Vect : T_Vecteur_Creux) return T_Vecteur_Creux is
    Curseur, Cellule_Prec, Nouvelle_Cellule : T_Vecteur_Creux;
    Copie                                   : T_Vecteur_Creux;
  begin
    Curseur := Vect;

    while Curseur /= null loop
      Nouvelle_Cellule :=
       new T_Cellule'
        (Ligne    => Curseur.all.Ligne, Valeur => Curseur.all.Valeur,
         Suivante => Curseur.all.Suivante, Precedente => Cellule_Prec,
         Ponderer => Curseur.all.Ponderer);

      if Cellule_Prec = null then
        Copie := Nouvelle_Cellule;
      else
        Cellule_Prec.all.Suivante := Nouvelle_Cellule;
      end if;

      Curseur      := Curseur.all.Suivante;
      Cellule_Prec := Nouvelle_Cellule;
    end loop;

    return Copie;
  end Copie_Vect_Creux;

  procedure Ponderation (Mat : in T_Matrice; Curseur_Mat : in T_Vecteur_Creux)
  is
  begin
    if Mat.Ponderer and then Curseur_Mat /= null
     and then Curseur_Mat.all.Ponderer
    then
      Curseur_Mat.all.Ponderer := False;
      Curseur_Mat.all.Valeur   :=
       Curseur_Mat.all.Valeur / Mat.Poids (Curseur_Mat.all.Ligne);
    end if;
  end Ponderation;

  procedure Init (Mat : out T_Matrice; Val : in T_Valeur := Zero) is

    procedure Init_Pleine is
    begin
      Mat.Matrice_Pleine := (others => (others => Val));
    end Init_Pleine;

    procedure Init_Creuse is
      Curseur_Cellule, Nouvelle_Cellule : T_Vecteur_Creux;
      Index_Ligne                       : Natural := 1;
    begin
      if Val = Zero then
        Mat.Matrice_Creuse := (others => null);
      else
        Curseur_Cellule        :=
         new T_Cellule'
          (Ligne    => 1, Valeur => Val, Suivante => null, Precedente => null,
           Ponderer => True);
        Mat.Matrice_Creuse (1) := Curseur_Cellule;

        while Index_Ligne < Mat.Lignes loop
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne      => Index_Ligne + 1, Valeur => Val, Suivante => null,
             Precedente => Curseur_Cellule, Ponderer => True);

          Curseur_Cellule.all.Suivante := Nouvelle_Cellule;
          Curseur_Cellule              := Nouvelle_Cellule;
          Index_Ligne                  := Index_Ligne + 1;
        end loop;

        for I in 2 .. Mat.Colonnes loop
          Mat.Matrice_Creuse (I) := Copie_Vect_Creux (Mat.Matrice_Creuse (1));
        end loop;

      end if;
    end Init_Creuse;

  begin
    if Mat.Pleine then
      Init_Pleine;
    else
      Init_Creuse;
    end if;
  end Init;

  procedure Init_Fichier (File : in File_Type; Mat : out T_Matrice) is
    I, J : Integer;

    procedure Init_Fichier_Plein is
      Sommet_Courant : Integer := 0;
    begin
      Init (Mat);
      Mat.Poids := (others => Zero);
      -- tant que le fichier n'est pas fini ou la ligne n'est pas vide
      while not End_Of_File (File) loop
        begin
          if End_Of_Line (File) then
            Skip_Line (File);
          end if;

          Get (File, I);

          if I + 1 /= Sommet_Courant then
            Sommet_Courant := I + 1;
          end if;
          Mat.Poids (Sommet_Courant) := Mat.Poids (Sommet_Courant) + Un;

          Get (File, J);
          Set (Mat, Sommet_Courant, J + 1, Un);
        exception
          when End_Error =>
            null;
        end;
      end loop;
    end Init_Fichier_Plein;

    procedure Init_Fichier_Creuse is
      Curseurs : array (1 .. Mat.Colonnes) of T_Vecteur_Creux :=
       (others => null);

      Sommet_Courant : Integer := 0;

      procedure Maj_Curseurs (Colonne : in Integer) is
      begin
        if Curseurs (Colonne) = null then -- le curseurs n'existe pas encore
          Curseurs (Colonne)           :=
           new T_Cellule'
            (Ligne      => Sommet_Courant, Valeur => Un, Suivante => null,
             Precedente => null, Ponderer => True);
          Mat.Matrice_Creuse (Colonne) := Curseurs (Colonne);

        elsif Curseurs (Colonne).all.Ligne = Sommet_Courant then
          -- on est sur le curseur
          Curseurs (Colonne).all.Valeur := Un;

        elsif Curseurs (Colonne).all.Ligne < Sommet_Courant
         and then
         (Curseurs (Colonne).all.Suivante = null
          or else Curseurs (Colonne).all.Suivante.all.Ligne > Sommet_Courant)
        then
          -- on est entre deux curseurs et le n'existe pas encore
          Curseurs (Colonne).all.Suivante :=
           new T_Cellule'
            (Ligne      => Sommet_Courant, Valeur => Un,
             Suivante   => Curseurs (Colonne).all.Suivante,
             Precedente => Curseurs (Colonne), Ponderer => True);
          Curseurs (Colonne)              := Curseurs (Colonne).all.Suivante;
        elsif Curseurs (Colonne).all.Ligne > Sommet_Courant
         and then
         (Curseurs (Colonne).all.Precedente = null -- au début
          or else Curseurs (Colonne).all.Precedente.all.Ligne < Sommet_Courant)
        then
          -- on est entre deux curseurs et le n'existe pas encore
          Curseurs (Colonne).all.Precedente :=
           new T_Cellule'
            (Ligne      => Sommet_Courant, Valeur => Un,
             Suivante   => Curseurs (Colonne),
             Precedente => Curseurs (Colonne).all.Precedente,
             Ponderer   => True);
          Curseurs (Colonne) := Curseurs (Colonne).all.Precedente;
          if Curseurs (Colonne).all.Precedente = null then
            Mat.Matrice_Creuse (Colonne) := Curseurs (Colonne);
          end if;
        elsif Curseurs (Colonne).Ligne < Sommet_Courant then
          -- on est avant le curseur
          -- on avance le curseur
          Curseurs (Colonne) := Curseurs (Colonne).all.Suivante;
          Maj_Curseurs (Colonne);

        elsif Curseurs (Colonne).all.Ligne > Sommet_Courant then
          -- on a dépasser le curseur
          if Sommet_Courant - Curseurs (Colonne).all.Ligne > (Mat.Lignes / 2)
          then
            -- on a dépasser le curseur de plus de la moitié de la matrice
            -- on repart donc du début
            Curseurs (Colonne) := Mat.Matrice_Creuse (Colonne);
            Maj_Curseurs (Colonne);
          else
            -- on est pas trop loin on recule donc le curseur
            Curseurs (Colonne) := Curseurs (Colonne).all.Precedente;
            Maj_Curseurs (Colonne);
          end if;
        end if;
      end Maj_Curseurs;

    begin
      -- Active le mode ponderation de la matrice
      Mat.Ponderer := True;
      Mat.Poids    := (others => Zero);

      while not End_Of_File (File) loop
        begin
          if End_Of_Line (File) then
            Skip_Line (File);
          end if;

          Get (File, I); -- on récupère l'arète actuelle

          if I + 1 /= Sommet_Courant then -- ie. on a changé d'arrête
            -- On change de sommet relecture des curseurs pour les ponderer
            --  Ponderer;

            Sommet_Courant := I + 1;
          end if;
          Mat.Poids (Sommet_Courant) := Mat.Poids (Sommet_Courant) + Un;

          Get (File, J); -- on récupère la colonne
          J := J + 1; -- on décale de 1 pour avoir un indice de 1 à n

          -- mets a jour le curseur
          Maj_Curseurs (J);
        exception
          when End_Error =>
            null;
        end;
      end loop;
      --  Ponderer;
    end Init_Fichier_Creuse;
  begin
    if Mat.Pleine then
      Init_Fichier_Plein;
    else
      Init_Fichier_Creuse;
    end if;
  end Init_Fichier;

  function Copie (Mat : in T_Matrice) return T_Matrice is
    Copie : T_Matrice (Mat.Lignes, Mat.Colonnes, Mat.Pleine);

    procedure Copier_Pleine is
    begin
      Copie.Matrice_Pleine := Mat.Matrice_Pleine;
    end Copier_Pleine;

    procedure Copier_Creuse is
    begin
      for I in 1 .. Mat.Colonnes loop
        Copie.Matrice_Creuse (I) := Copie_Vect_Creux (Mat.Matrice_Creuse (I));
      end loop;
    end Copier_Creuse;

  begin
    if Mat.Pleine then
      Copier_Pleine;
    else
      Copier_Creuse;
    end if;

    Copie.Ponderer := True;
    Copie.Poids    := Mat.Poids;

    return Copie;
  end Copie;

  procedure Detruire (Mat : in out T_Matrice) is

    procedure Detruire_Vect_Creux (Vect : in T_Vecteur_Creux) is
      Curseur, Suivant : T_Vecteur_Creux;
    begin
      Curseur := Vect;

      while Curseur /= null loop
        Suivant := Curseur.all.Suivante;
        Free (Curseur);
        Curseur := Suivant;
      end loop;
    end Detruire_Vect_Creux;

  begin
    if Mat.Pleine then
      null;
    else
      for I in 1 .. Mat.Colonnes loop
        Detruire_Vect_Creux (Mat.Matrice_Creuse (I));
      end loop;
    end if;
  end Detruire;

  procedure TrouverParRecursion_V
   (Vect    : in     T_Vecteur_Creux; Ligne : in Positive;
    Cellule :    out T_Vecteur_Creux)
  is
  begin
    if Vect = null or else Vect.all.Ligne > Ligne then
      Cellule := null;
    elsif Vect.all.Ligne = Ligne then
      Cellule := Vect;
    else
      TrouverParRecursion_V (Vect.all.Suivante, Ligne, Cellule);
    end if;
  end TrouverParRecursion_V;

  function Get (Mat : in T_Matrice; Ligne, Colonne : Positive) return T_Valeur
  is
  begin
    if Mat.Pleine then
      return Mat.Matrice_Pleine (Ligne, Colonne) / Mat.Poids (Ligne);
    else
      declare
        Cellule : T_Vecteur_Creux;
      begin
        TrouverParRecursion_V (Mat.Matrice_Creuse (Colonne), Ligne, Cellule);

        if Cellule = null then
          return Zero;
        else
          Ponderation (Mat, Cellule);
          return Cellule.all.Valeur;
        end if;
      end;
    end if;
  end Get;

  function Get_Poids (Mat : in T_Matrice; Ligne : Positive) return T_Valeur is
  begin
    return Mat.Poids (Ligne);
  end Get_Poids;

  procedure Set_Poids
   (Mat : in out T_Matrice; Ligne : Positive; Val : in T_Valeur)
  is
  begin
    Mat.Poids (Ligne) := Val;
  end Set_Poids;

  procedure TrouverParRecursion_V
   (Vect : in     T_Vecteur_Creux; Ligne : in Positive;
    Cellule, Cellule_Prec, Cellule_Suiv :    out T_Vecteur_Creux)
  is
  begin
    if Vect = null then
      Cellule := null;
    elsif Vect.all.Ligne = Ligne then
      Cellule := Vect;
    elsif Vect.all.Ligne > Ligne then
      Cellule      := null;
      Cellule_Suiv := Vect;
    else
      Cellule_Prec := Vect;
      Cellule_Suiv := Vect.all.Suivante;
      TrouverParRecursion_V
       (Vect.all.Suivante, Ligne, Cellule, Cellule_Prec, Cellule_Suiv);
    end if;
  end TrouverParRecursion_V;

  procedure Set
   (Mat : in out T_Matrice; Ligne, Colonne : Positive; Val : in T_Valeur)
  is
    procedure Set_Pleine is
    begin
      Mat.Matrice_Pleine (Ligne, Colonne) := Val;
    end Set_Pleine;

    procedure Set_Creuse is
      Cellule, Cellule_Prec, Cellule_Suiv, Nouvelle_Cellule : T_Vecteur_Creux;
    begin
      TrouverParRecursion_V
       (Mat.Matrice_Creuse (Colonne), Ligne, Cellule, Cellule_Prec,
        Cellule_Suiv);

      if Val = Zero then
        if Cellule /= null then
          if Cellule_Prec = null then
            Mat.Matrice_Creuse (Colonne) := Cellule.all.Suivante;
          else
            Cellule_Prec.all.Suivante := Cellule.all.Suivante;
          end if;
          Free (Cellule);
        end if;
      else
        if Cellule = null then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne      => Ligne, Valeur => Val, Suivante => Cellule_Suiv,
             Precedente => Cellule_Prec, Ponderer => False);
          if Cellule_Prec = null then
            Mat.Matrice_Creuse (Colonne) := Nouvelle_Cellule;
            Mat.Ponderer                 :=
             False; -- La matrice a été modifié, la ponderation n'est plus valable
          else
            Cellule_Prec.all.Suivante := Nouvelle_Cellule;
          end if;
        else
          Cellule.all.Valeur := Val;
        end if;
      end if;
    end Set_Creuse;

  begin
    if Mat.Pleine then
      Set_Pleine;
    else
      Set_Creuse;
    end if;
  end Set;

  function "+" (A, B : in T_Matrice) return T_Matrice is
    Mat : T_Matrice (A.Lignes, A.Colonnes, A.Pleine);

    procedure Addition_Pleine is
    begin
      for I in 1 .. A.Lignes loop
        for J in 1 .. A.Colonnes loop
          Set (Mat, I, J, Get (A, I, J) + Get (B, I, J));
        end loop;
      end loop;
    end Addition_Pleine;

    function Addition_Creuse_V
     (Vect_A, Vect_B : in T_Vecteur_Creux) return T_Vecteur_Creux
    is
      Curseur_A, Curseur_B, Cellule_Prec, Nouvelle_Cellule : T_Vecteur_Creux;
      Vect                                                 : T_Vecteur_Creux;
    begin
      Curseur_A := Vect_A;
      Curseur_B := Vect_B;

      while Curseur_A /= null or Curseur_B /= null loop
        Ponderation (A, Curseur_A);
        Ponderation (B, Curseur_B);

        if Curseur_A = null and Curseur_B /= null then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Curseur_B.all.Ligne, Valeur => Curseur_B.all.Valeur,
             Suivante => null, Precedente => Cellule_Prec, Ponderer => True);

          Curseur_B := Curseur_B.all.Suivante;
        elsif Curseur_A /= null and Curseur_B = null then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Curseur_A.all.Ligne, Valeur => Curseur_A.all.Valeur,
             Suivante => null, Precedente => Cellule_Prec, Ponderer => True);

          Curseur_A := Curseur_A.all.Suivante;
        elsif Curseur_A.all.Ligne < Curseur_B.all.Ligne then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Curseur_A.all.Ligne, Valeur => Curseur_A.all.Valeur,
             Suivante => null, Precedente => Cellule_Prec, Ponderer => True);

          Curseur_A := Curseur_A.all.Suivante;
        elsif Curseur_A.all.Ligne > Curseur_B.all.Ligne then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Curseur_B.all.Ligne, Valeur => Curseur_B.all.Valeur,
             Suivante => null, Precedente => Cellule_Prec, Ponderer => True);

          Curseur_B := Curseur_B.all.Suivante;
        else
          if Curseur_A.all.Valeur + Curseur_B.all.Valeur = Zero then
            null;
          else
            Nouvelle_Cellule :=
             new T_Cellule'
              (Ligne    => Curseur_A.all.Ligne,
               Valeur   => Curseur_A.all.Valeur + Curseur_B.all.Valeur,
               Suivante => null, Precedente => Cellule_Prec, Ponderer => True);
          end if;

          Curseur_A := Curseur_A.all.Suivante;
          Curseur_B := Curseur_B.all.Suivante;
        end if;

        if Nouvelle_Cellule /= null then
          if Cellule_Prec = null then
            Vect := Nouvelle_Cellule;
          else
            Cellule_Prec.all.Suivante := Nouvelle_Cellule;
          end if;
          Cellule_Prec := Nouvelle_Cellule;
        end if;
        Nouvelle_Cellule := null;
      end loop;
      return Vect;
    end Addition_Creuse_V;

    procedure Addition_Creuse is
    begin
      for I in 1 .. A.Colonnes loop
        Mat.Matrice_Creuse (I) :=
         Addition_Creuse_V (A.Matrice_Creuse (I), B.Matrice_Creuse (I));
      end loop;
    end Addition_Creuse;
  begin
    -- Init(Mat, 0);
    if Mat.Pleine then
      Addition_Pleine;
    else
      Addition_Creuse;
    end if;

    return Mat;
  end "+";

  function "*" (A, B : in T_Matrice) return T_Matrice is
    Pleine : constant Boolean := A.Pleine or B.Pleine;
    Mat    : T_Matrice (A.Lignes, B.Colonnes, Pleine);

    procedure Multiplication_Pleine is
    begin
      for I in 1 .. Mat.Lignes loop
        for J in 1 .. Mat.Colonnes loop
          Set (Mat, I, J, Get (A, I, 1) * Get (B, 1, J));
          for K in 2 .. A.Colonnes loop
            Set (Mat, I, J, Get (Mat, I, J) + Get (A, I, K) * Get (B, K, J));
          end loop;
        end loop;
      end loop;
    end Multiplication_Pleine;

    function Produit_Scalaire
     (Mat_A  : in T_Matrice; Vect_A : in Integer; Mat_B : in T_Matrice;
      Vect_B : in Integer) return T_Valeur
    is
      Curseur_A, Curseur_B : T_Vecteur_Creux;
      Accu                 : T_Valeur := Zero;
    begin
      Curseur_A := Mat_A.Matrice_Creuse (Vect_A);
      Curseur_B := Mat_B.Matrice_Creuse (Vect_B);

      while Curseur_A /= null or Curseur_B /= null loop
        Ponderation (Mat_A, Curseur_A);
        Ponderation (Mat_B, Curseur_B);

        if Curseur_A = null and Curseur_B /= null then
          Curseur_B := null;
        elsif Curseur_A /= null and Curseur_B = null then
          Curseur_A := null;
        elsif Curseur_A.all.Ligne < Curseur_B.all.Ligne then
          Curseur_A := Curseur_A.all.Suivante;
        elsif Curseur_A.all.Ligne > Curseur_B.all.Ligne then
          Curseur_B := Curseur_B.all.Suivante;
        else
          Accu := Accu + Curseur_A.all.Valeur * Curseur_B.all.Valeur;

          Curseur_A := Curseur_A.all.Suivante;
          Curseur_B := Curseur_B.all.Suivante;
        end if;
      end loop;
      return Accu;
    end Produit_Scalaire;

    procedure Multiplication_Creuse is
      Cellule_Prec, Nouvelle_Cellule : T_Vecteur_Creux;
      A_Transpose : T_Matrice (A.Colonnes, A.Lignes, A.Pleine) :=
       Transpose (A);
    begin
      for Index_B in 1 .. B.Colonnes loop
        Cellule_Prec := null;
        for Index_A in 1 .. A.Lignes loop
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Index_A,
             Valeur   => Produit_Scalaire (A_Transpose, Index_A, B, Index_B),
             Suivante => null, Precedente => Cellule_Prec, Ponderer => False);
          if Cellule_Prec = null then
            Mat.Matrice_Creuse (Index_B) := Nouvelle_Cellule;
          else
            Cellule_Prec.all.Suivante := Nouvelle_Cellule;
          end if;
          Cellule_Prec := Nouvelle_Cellule;
        end loop;
      end loop;
      Detruire (A_Transpose);
    end Multiplication_Creuse;

    procedure Multiplication_Mixte is
      Curseur     : T_Vecteur_Creux;
      A_Transpose : T_Matrice (A.Colonnes, A.Lignes, A.Pleine);
      Produit     : T_Valeur;
    begin
      Init (Mat, Zero);
      if A.Pleine then
        for I in 1 .. A.Lignes loop
          for J in 1 .. B.Colonnes loop
            Curseur := B.Matrice_Creuse (J);
            Produit :=
             Zero; -- On utilise une variable pour éviter de faire des Get, Set inutiles
            while Curseur /= null loop
              Ponderation (B, Curseur);
              Produit :=
               Produit + Get (A, I, Curseur.all.Ligne) * Curseur.all.Valeur;
              Curseur := Curseur.all.Suivante;
            end loop;

            Set (Mat, I, J, Produit);
          end loop;
        end loop;
      else
        A_Transpose := Transpose (A); -- pas de ponderation maybe broken
        for I in 1 .. A.Lignes loop
          for J in 1 .. B.Colonnes loop
            Curseur := A_Transpose.Matrice_Creuse (I);
            Produit :=
             Zero; -- On utilise une variable pour éviter de faire des Get, Set inutiles
            while Curseur /= null loop
              Produit :=
               Produit + Get (B, Curseur.all.Ligne, J) * Curseur.all.Valeur;
              Curseur := Curseur.all.Suivante;
            end loop;

            Set (Mat, I, J, Produit);
          end loop;
        end loop;

        Detruire (A_Transpose);
      end if;

    end Multiplication_Mixte;

  begin
    if A.Pleine and B.Pleine then
      Multiplication_Pleine;
    elsif (A.Pleine and not B.Pleine) or (B.Pleine and not A.Pleine) then
      Multiplication_Mixte;
    else
      Multiplication_Creuse;
    end if;

    return Mat;
  end "*";

  function Multiplication_Creuse_V
   (Mat : T_Matrice; Vect : Integer; B : in T_Valeur) return T_Vecteur_Creux
  is
    Curseur, Cellule_Prec, Nouvelle_Cellule : T_Vecteur_Creux;
    Copie                                   : T_Vecteur_Creux;
  begin
    Curseur := Mat.Matrice_Creuse (Vect);
    while Curseur /= null loop
      Ponderation (Mat, Curseur);
      Nouvelle_Cellule :=
       new T_Cellule'
        (Ligne    => Curseur.all.Ligne, Valeur => Curseur.all.Valeur * B,
         Suivante => Curseur.all.Suivante, Precedente => Cellule_Prec,
         Ponderer => False);
      if Cellule_Prec = null then
        Copie := Nouvelle_Cellule;
      else
        Cellule_Prec.all.Suivante := Nouvelle_Cellule;
      end if;
      Curseur      := Curseur.all.Suivante;
      Cellule_Prec := Nouvelle_Cellule;
    end loop;
    return Copie;
  end Multiplication_Creuse_V;

  function "*" (A : in T_Matrice; B : in T_Valeur) return T_Matrice is
    Mat : T_Matrice (A.Lignes, A.Colonnes, A.Pleine);

    procedure Multiplication_Plein is
    begin
      for I in 1 .. A.Lignes loop
        for J in 1 .. A.Colonnes loop
          Set (Mat, I, J, Get (A, I, J) * B);
        end loop;
      end loop;
    end Multiplication_Plein;

    procedure Multiplication_Creuse is
    begin
      if B = Zero then
        Detruire (Mat);
      else
        for I in 1 .. A.Colonnes loop
          Mat.Matrice_Creuse (I) := Multiplication_Creuse_V (A, I, B);
        end loop;
      end if;
    end Multiplication_Creuse;

  begin
    if Mat.Pleine then
      Multiplication_Plein;
    else
      Multiplication_Creuse;
    end if;

    return Mat;
  end "*";

  function "*" (A : in T_Valeur; B : in T_Matrice) return T_Matrice is
    Mat : T_Matrice (B.Lignes, B.Colonnes, B.Pleine);

    --  procedure Traiter (Ligne, Colonne : in Positive; Val : in out T_Valeur) is begin
    --    Val := Val * B;
    --  end Traiter;

    --  procedure Multiplication is new Pour_Chaque (Traiter);
    -- Idéal car 1 seul parcours mais pose problème si on veut supprimer quand valeur = Zero

    procedure Multiplication_Plein is
    begin
      for I in 1 .. B.Lignes loop
        for J in 1 .. B.Colonnes loop
          Set (Mat, I, J, Get (B, I, J) * A);
        end loop;
      end loop;
    end Multiplication_Plein;

    procedure Multiplication_Creuse is
    begin
      if A = Zero then
        Detruire (Mat);
      else
        for I in 1 .. B.Colonnes loop
          Mat.Matrice_Creuse (I) := Multiplication_Creuse_V (B, I, A);
        end loop;
      end if;
    end Multiplication_Creuse;

  begin
    if Mat.Pleine then
      Multiplication_Plein;
    else
      Multiplication_Creuse;
    end if;

    return Mat;
  end "*";

  function Somme (Mat : T_Matrice) return T_Valeur is
    Total : T_Valeur := Zero;
  begin
    if Mat.Pleine then
      for I in 1 .. Mat.Lignes loop
        for J in 1 .. Mat.Colonnes loop
          Total := Total + Get (Mat, I, J);
        end loop;
      end loop;
    else
      declare
        Curseur : T_Vecteur_Creux;
      begin
        for I in 1 .. Mat.Colonnes loop
          Curseur := Mat.Matrice_Creuse (I);
          while Curseur /= null loop
            Ponderation (Mat, Curseur);
            Total   := Total + Curseur.all.Valeur;
            Curseur := Curseur.all.Suivante;
          end loop;
        end loop;
      end;
    end if;
    return Total;
  end Somme;

  procedure PageRankIter
   (Pi : in out T_Matrice; G : in T_Matrice; Alpha, N : in T_Valeur)
  is
    NewPi      : T_Matrice (Pi.Lignes, Pi.Colonnes, Pi.Pleine);
    Curseur    : T_Vecteur_Creux;
    Produit    : T_Valeur;
    Alpha_N    : constant T_Valeur := Alpha / N;
    Default_Pi : constant T_Valeur := Somme (Pi) * ((Un - Alpha) / N);
  begin
    if G.Pleine then
      Pi := Pi * G;
    else
      for J in 1 .. G.Colonnes loop
        Curseur := G.Matrice_Creuse (J);
        Produit :=
         Zero; -- On utilise une variable pour éviter de faire des Get, Set inutiles
        for k in 1 .. G.Lignes loop

          if Curseur /= null and then Curseur.all.Ligne = k
          then -- curseur existe
            Ponderation (G, Curseur);
            Produit := Produit + Get (Pi, 1, k) * (Alpha * Curseur.all.Valeur);
            Curseur := Curseur.all.Suivante;

          elsif Get_Poids (G, k) = Zero then
            Produit := Produit + (Get (Pi, 1, k) * Alpha_N);
          end if;
        end loop;
        Set (NewPi, 1, J, Default_Pi + Produit);
      end loop;
      Pi := NewPi;
    end if;
  end PageRankIter;

  procedure Pour_Chaque (Mat : in T_Matrice) is
    procedure Pour_Chaque_Pleine is
      Matrice_Pleine : T_Matrice_Pleine := Mat.Matrice_Pleine;
    begin
      for I in Matrice_Pleine'Range (1) loop
        for J in Matrice_Pleine'Range (2) loop
          Traiter (I, J, Matrice_Pleine (I, J));
        end loop;
      end loop;
    end Pour_Chaque_Pleine;

    procedure Pour_Chaque_Creuse is
      Curseur_Cellule : T_Vecteur_Creux;
    begin
      for I in 1 .. Mat.Colonnes loop
        Curseur_Cellule := Mat.Matrice_Creuse (I);
        while Curseur_Cellule /= null loop
          Traiter (Curseur_Cellule.all.Ligne, I, Curseur_Cellule.all.Valeur);
          Curseur_Cellule := Curseur_Cellule.all.Suivante;
        end loop;
      end loop;
    end Pour_Chaque_Creuse;
  begin
    if Mat.Pleine then
      Pour_Chaque_Pleine;
    else
      Pour_Chaque_Creuse;
    end if;
  end Pour_Chaque;

  function Transpose (A : in T_Matrice) return T_Matrice is
    Mat :
     T_Matrice
      (Lignes => A.Colonnes, Colonnes => A.Lignes, Pleine => A.Pleine);

    function Transpose_Pleine return T_Matrice is
    begin
      for I in 1 .. A.Lignes loop
        for J in 1 .. A.Colonnes loop
          Set (Mat, J, I, Get (A, I, J));
        end loop;
      end loop;

      return Mat;
    end Transpose_Pleine;

    function Transpose_Creuse return T_Matrice is
      Curseurs          : array (1 .. A.Colonnes) of T_Vecteur_Creux :=
       (others => null);
      Curseur_Transpose : T_Vecteur_Creux;
    begin
      -- Initialisation des curseurs
      for I in 1 .. A.Colonnes loop
        Curseurs (I) := A.Matrice_Creuse (I);
      end loop;

      for I in 1 .. A.Lignes loop
        for J in 1 .. A.Colonnes loop
          if Curseurs (J) /= null and then Curseurs (J).all.Ligne = I then
            Ponderation (A, Curseurs (J));

            if Curseur_Transpose = null then
              Curseur_Transpose      :=
               new T_Cellule'
                (Ligne    => J, Valeur => Curseurs (J).all.Valeur,
                 Suivante => null, Precedente => null, Ponderer => False);
              Mat.Matrice_Creuse (I) := Curseur_Transpose;
            else
              Curseur_Transpose.all.Suivante :=
               new T_Cellule'
                (Ligne    => J, Valeur => Curseurs (J).all.Valeur,
                 Suivante => null, Precedente => Curseur_Transpose,
                 Ponderer => False);
              Curseur_Transpose              := Curseur_Transpose.all.Suivante;
            end if;
            Curseurs (J) := Curseurs (J).all.Suivante;
          end if;
        end loop;
        Curseur_Transpose := null;
      end loop;

      --  for I in 1 .. A.Colonnes loop
      --    Curseur_Cellule := A.Matrice_Creuse (I);
      --    while Curseur_Cellule /= null loop
      --      Set (Mat, I, Curseur_Cellule.all.Ligne, Curseur_Cellule.all.Valeur);
      --      Curseur_Cellule := Curseur_Cellule.all.Suivante;
      --    end loop;
      --  end loop;

      return Mat;
    end Transpose_Creuse;

  begin
    if A.Pleine then
      return Transpose_Pleine;
    else
      return Transpose_Creuse;
    end if;
  end Transpose;

  procedure Afficher (Mat : in T_Matrice) is
  begin
    --  Put_Line(Header);
    for I in 1 .. Mat.Lignes loop
      Put ("│");
      for J in 1 .. Mat.Colonnes loop
        Afficher_Valeur (Get (Mat, I, J));
        Put (" ");
      end loop;
      Put_Line ("│");
    end loop;
    --  Put_Line(Footer);
  end Afficher;

end Matrice;
