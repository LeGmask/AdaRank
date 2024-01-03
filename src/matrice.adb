--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
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
         Suivante => Curseur.all.Suivante);

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
         new T_Cellule'(Ligne => 1, Valeur => Val, Suivante => null);
        Mat.Matrice_Creuse (1) := Curseur_Cellule;

        while Index_Ligne < Mat.Lignes loop
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne => Index_Ligne + 1, Valeur => Val, Suivante => null);

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
      return Mat.Matrice_Pleine (Ligne, Colonne);
    else
      declare
        Cellule : T_Vecteur_Creux;
      begin
        TrouverParRecursion_V (Mat.Matrice_Creuse (Colonne), Ligne, Cellule);

        if Cellule = null then
          return Zero;
        else
          return Cellule.all.Valeur;
        end if;
      end;
    end if;
  end Get;

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
            (Ligne => Ligne, Valeur => Val, Suivante => Cellule_Suiv);
          if Cellule_Prec = null then
            Mat.Matrice_Creuse (Colonne) := Nouvelle_Cellule;
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
        if Curseur_A = null and Curseur_B /= null then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Curseur_B.all.Ligne, Valeur => Curseur_B.all.Valeur,
             Suivante => null);

          Curseur_B := Curseur_B.all.Suivante;
        elsif Curseur_A /= null and Curseur_B = null then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Curseur_A.all.Ligne, Valeur => Curseur_A.all.Valeur,
             Suivante => null);

          Curseur_A := Curseur_A.all.Suivante;
        elsif Curseur_A.all.Ligne < Curseur_B.all.Ligne then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Curseur_A.all.Ligne, Valeur => Curseur_A.all.Valeur,
             Suivante => null);

          Curseur_A := Curseur_A.all.Suivante;
        elsif Curseur_A.all.Ligne > Curseur_B.all.Ligne then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne    => Curseur_B.all.Ligne, Valeur => Curseur_B.all.Valeur,
             Suivante => null);

          Curseur_B := Curseur_B.all.Suivante;
        else
          if Curseur_A.all.Valeur + Curseur_B.all.Valeur = Zero then
            null;
          else
            Nouvelle_Cellule :=
             new T_Cellule'
              (Ligne    => Curseur_A.all.Ligne,
               Valeur   => Curseur_A.all.Valeur + Curseur_B.all.Valeur,
               Suivante => null);
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
    Mat : T_Matrice (A.Lignes, B.Colonnes, A.Pleine);

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
     (Vect_A, Vect_B : in T_Vecteur_Creux) return T_Valeur
    is
      Curseur_A, Curseur_B : T_Vecteur_Creux;
      Accu                 : T_Valeur := Zero;
    begin
      Curseur_A := Vect_A;
      Curseur_B := Vect_B;

      while Curseur_A /= null or Curseur_B /= null loop
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
             Valeur   =>
              Produit_Scalaire
               (A_Transpose.Matrice_Creuse (Index_A),
                B.Matrice_Creuse (Index_B)),
             Suivante => null);
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
  begin
    if Mat.Pleine then
      Multiplication_Pleine;
    else
      Multiplication_Creuse;
    end if;

    return Mat;
  end "*";

  function Multiplication_Creuse_V
   (Vect : T_Vecteur_Creux; B : in T_Valeur) return T_Vecteur_Creux
  is
    Curseur, Cellule_Prec, Nouvelle_Cellule : T_Vecteur_Creux;
    Copie                                   : T_Vecteur_Creux;
  begin
    Curseur := Vect;
    while Curseur /= null loop
      Nouvelle_Cellule :=
       new T_Cellule'
        (Ligne    => Curseur.all.Ligne, Valeur => Curseur.all.Valeur * B,
         Suivante => Curseur.all.Suivante);
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

    --  procedure Traiter (Ligne, Colonne : in Positive; Val : in out T_Valeur) is begin
    --    Val := Val * B;
    --  end Traiter;

    --  procedure Multiplication is new Pour_Chaque (Traiter);
    -- Idéal car 1 seul parcours mais pose problème si on veut supprimer quand valeur = Zero

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
          Mat.Matrice_Creuse (I) :=
           Multiplication_Creuse_V (A.Matrice_Creuse (I), B);
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
          Mat.Matrice_Creuse (I) :=
           Multiplication_Creuse_V (B.Matrice_Creuse (I), A);
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
      Curseur_Cellule : T_Vecteur_Creux;
    begin
      for I in 1 .. A.Colonnes loop
        Curseur_Cellule := A.Matrice_Creuse (I);
        while Curseur_Cellule /= null loop
          Set (Mat, I, Curseur_Cellule.all.Ligne, Curseur_Cellule.all.Valeur);
          Curseur_Cellule := Curseur_Cellule.all.Suivante;
        end loop;
      end loop;

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
