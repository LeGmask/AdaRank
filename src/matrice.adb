--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Matrice is

  procedure Free_Cellule is new Ada.Unchecked_Deallocation
   (Object => T_Cellule, Name => T_Vecteur_Creux);

  procedure Free_Colonne is new Ada.Unchecked_Deallocation
   (Object => T_Colonne, Name => T_Matrice_Creuse);

  procedure Init (Mat : out T_Matrice; Val : in T_Valeur := Zero) is

    procedure Init_Pleine is
    begin
      Mat.Matrice_Pleine := (others => (others => Val));
    end Init_Pleine;

    procedure Init_Creuse is
      Curseur_Cellule, Nouvelle_Cellule : T_Vecteur_Creux;
      Curseur_Colonne,
      Nouvelle_Colonne : T_Matrice (Mat.Lignes, Mat.Colonnes, False);
      Index_Colonne                     : Natural := 1;
      Index_Ligne                       : Natural := 1;
    begin
      if Val = Zero then
        Mat.Matrice_Creuse := null;
      else
        Curseur_Cellule :=
         new T_Cellule'(Ligne => 1, Valeur => Val, Suivante => null);

        Curseur_Colonne.Matrice_Creuse :=
         new T_Colonne'
          (Colonne => 1, Suivante => null, Vecteur => Curseur_Cellule);

        Mat := Curseur_Colonne;

        while Index_Ligne < Mat.Lignes loop
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne => Index_Ligne + 1, Valeur => Val, Suivante => null);

          Curseur_Cellule.all.Suivante := Nouvelle_Cellule;
          Curseur_Cellule              := Nouvelle_Cellule;
          Index_Ligne                  := Index_Ligne + 1;
        end loop;

        while Index_Colonne < Mat.Colonnes loop
          Nouvelle_Colonne := Copie (Curseur_Colonne);
          Nouvelle_Colonne.Matrice_Creuse.all.Colonne :=
           Nouvelle_Colonne.Matrice_Creuse.all.Colonne + 1;

          Curseur_Colonne.Matrice_Creuse.all.Suivante :=
           Nouvelle_Colonne.Matrice_Creuse;
          Curseur_Colonne                             := Nouvelle_Colonne;
          Index_Colonne                               := Index_Colonne + 1;
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

  function Copie (Mat : in T_Matrice) return T_Matrice is
    Copie : T_Matrice (Mat.Lignes, Mat.Colonnes, Mat.Pleine);

    procedure Copier_Pleine is
    begin
      Copie.Matrice_Pleine := Mat.Matrice_Pleine;
    end Copier_Pleine;

    procedure Copier_Creuse is
      Curseur, Colonne_Prec, Nouvelle_Colonne : T_Matrice_Creuse;
    begin
      Curseur := Mat.Matrice_Creuse;

      while Curseur /= null loop
        Nouvelle_Colonne :=
         new T_Colonne'
          (Colonne => Curseur.all.Colonne, Suivante => Curseur.all.Suivante,
           Vecteur => Copie_Vect_Creux (Curseur.all.Vecteur));

        if Colonne_Prec = null then
          Copie.Matrice_Creuse := Nouvelle_Colonne;
        else
          Colonne_Prec.all.Suivante := Nouvelle_Colonne;
        end if;

        Curseur      := Curseur.all.Suivante;
        Colonne_Prec := Nouvelle_Colonne;
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
        Free_Cellule (Curseur);
        Curseur := Suivant;
      end loop;
    end Detruire_Vect_Creux;

  begin
    if Mat.Pleine then
      null;
    else
      declare
        Curseur, Suivant : T_Matrice_Creuse;
      begin
        Curseur := Mat.Matrice_Creuse;

        while Curseur /= null loop
          Suivant := Curseur.all.Suivante;
          Detruire_Vect_Creux (Curseur.all.Vecteur);
          Free_Colonne (Curseur);
          Curseur := Suivant;
        end loop;
      end;
    end if;
  end Detruire;

  procedure TrouverParRecursion
   (Col     : in     T_Matrice_Creuse; Ligne, Colonne : in Positive;
    Cellule :    out T_Vecteur_Creux)
  is
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

  begin
    if Col = null or else Col.all.Colonne > Colonne then
      Cellule := null;
    elsif Col.all.Colonne = Colonne then
      TrouverParRecursion_V (Col.all.Vecteur, Ligne, Cellule);
    else
      TrouverParRecursion (Col.all.Suivante, Ligne, Colonne, Cellule);
    end if;
  end TrouverParRecursion;

  function Get (Mat : in T_Matrice; Ligne, Colonne : Positive) return T_Valeur
  is
  begin
    if Mat.Pleine then
      return Mat.Matrice_Pleine (Ligne, Colonne);
    else
      declare
        Cellule : T_Vecteur_Creux;
      begin
        TrouverParRecursion (Mat.Matrice_Creuse, Ligne, Colonne, Cellule);

        if Cellule = null then
          return Zero;
        else
          return Cellule.all.Valeur;
        end if;
      end;
    end if;
  end Get;

  procedure TrouverParRecursion
   (Matrice : in     T_Matrice_Creuse; Ligne, Colonne : in Positive;
    Cellule, Cellule_Prec, Cellule_Suiv :    out T_Vecteur_Creux;
    Col, Col_Prec, Col_Suiv             :    out T_Matrice_Creuse)
  is
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
  begin
    if Matrice = null then
      Col := null;
    elsif Matrice.all.Colonne = Colonne then
      TrouverParRecursion_V
       (Matrice.all.Vecteur, Ligne, Cellule, Cellule_Prec, Cellule_Suiv);
      Col := Matrice;
    elsif Matrice.all.Colonne > Colonne then
      Col      := null;
      Col_Suiv := Matrice;
    else
      Col_Prec := Matrice;
      Col_Suiv := Matrice.all.Suivante;
      TrouverParRecursion
       (Matrice.all.Suivante, Ligne, Colonne, Cellule, Cellule_Prec,
        Cellule_Suiv, Col, Col_Prec, Col_Suiv);
    end if;
  end TrouverParRecursion;

  procedure Set
   (Mat : in out T_Matrice; Ligne, Colonne : Positive; Val : in T_Valeur)
  is
    procedure Set_Pleine is
    begin
      Mat.Matrice_Pleine (Ligne, Colonne) := Val;
    end Set_Pleine;

    procedure Set_Creuse is
      Col, Col_Prec, Col_Suiv, Nouvelle_Col                 : T_Matrice_Creuse;
      Cellule, Cellule_Prec, Cellule_Suiv, Nouvelle_Cellule : T_Vecteur_Creux;
    begin
      TrouverParRecursion
       (Mat.Matrice_Creuse, Ligne, Colonne, Cellule, Cellule_Prec,
        Cellule_Suiv, Col, Col_Prec, Col_Suiv);

      if Val = Zero then
        if Col /= null then
          if Cellule /= null then
            if Cellule_Prec = null then
              Col.all.Vecteur := Cellule.all.Suivante;
            else
              Cellule_Prec.all.Suivante := Cellule.all.Suivante;
            end if;
            Free_Cellule (Cellule);
          end if;

          if Col.all.Vecteur = null then
            if Col_Prec = null then
              Mat.Matrice_Creuse := Col.all.Suivante;
            else
              Col_Prec.all.Suivante := Col.all.Suivante;
            end if;
            Free_Colonne (Col);
          end if;
        end if;
      else
        if Cellule = null then
          if Col = null then
            Nouvelle_Cellule :=
             new T_Cellule'(Ligne => Ligne, Valeur => Val, Suivante => null);
            Nouvelle_Col     :=
             new T_Colonne'
              (Colonne => Colonne, Suivante => Col_Suiv,
               Vecteur => Nouvelle_Cellule);

            if Col_Prec = null then
              Mat.Matrice_Creuse := Nouvelle_Col;
            else
              Col_Prec.all.Suivante := Nouvelle_Col;
            end if;
          else
            Nouvelle_Cellule :=
             new T_Cellule'
              (Ligne => Ligne, Valeur => Val, Suivante => Cellule_Suiv);

            if Cellule_Prec = null then
              Col.all.Vecteur := Nouvelle_Cellule;
            else
              Cellule_Prec.all.Suivante := Nouvelle_Cellule;
            end if;
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
      Curseur_A, Curseur_B, Colonne_Prec, Nouvelle_Colonne : T_Matrice_Creuse;
    begin
      Curseur_A := A.Matrice_Creuse;
      Curseur_B := B.Matrice_Creuse;

      while Curseur_A /= null or Curseur_B /= null loop
        if Curseur_A = null and Curseur_B /= null then
          Nouvelle_Colonne :=
           new T_Colonne'
            (Colonne => Curseur_B.all.Colonne, Suivante => null,
             Vecteur => Copie_Vect_Creux (Curseur_B.all.Vecteur));
          Curseur_B        := Curseur_B.all.Suivante;
        elsif Curseur_A /= null and Curseur_B = null then
          Nouvelle_Colonne :=
           new T_Colonne'
            (Colonne => Curseur_A.all.Colonne, Suivante => null,
             Vecteur => Copie_Vect_Creux (Curseur_A.all.Vecteur));
          Curseur_A        := Curseur_A.all.Suivante;
        elsif Curseur_A.all.Colonne < Curseur_B.all.Colonne then
          Nouvelle_Colonne :=
           new T_Colonne'
            (Colonne => Curseur_A.all.Colonne, Suivante => null,
             Vecteur => Copie_Vect_Creux (Curseur_A.all.Vecteur));
          Curseur_A        := Curseur_A.all.Suivante;
        elsif Curseur_A.all.Colonne > Curseur_B.all.Colonne then
          Nouvelle_Colonne :=
           new T_Colonne'
            (Colonne => Curseur_B.all.Colonne, Suivante => null,
             Vecteur => Copie_Vect_Creux (Curseur_B.all.Vecteur));
          Curseur_B        := Curseur_B.all.Suivante;
        else
          Nouvelle_Colonne :=
           new T_Colonne'
            (Colonne => Curseur_A.all.Colonne, Suivante => null,
             Vecteur =>
              Addition_Creuse_V
               (Curseur_A.all.Vecteur, Curseur_B.all.Vecteur));
          Curseur_A        := Curseur_A.all.Suivante;
          Curseur_B        := Curseur_B.all.Suivante;
        end if;

        if Nouvelle_Colonne /= null then
          if Colonne_Prec = null then
            Mat.Matrice_Creuse := Nouvelle_Colonne;
          else
            Colonne_Prec.all.Suivante := Nouvelle_Colonne;
          end if;
          Colonne_Prec := Nouvelle_Colonne;
        end if;
        Nouvelle_Colonne := null;
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
      Curseur_A, Curseur_B, Colonne_Prec, Nouvelle_Colonne : T_Matrice_Creuse;
      Cellule_Prec, Nouvelle_Cellule                       : T_Vecteur_Creux;
      A_Transpose : T_Matrice (A.Colonnes, A.Lignes, A.Pleine) :=
       Transpose (A);
    begin
      Curseur_B := B.Matrice_Creuse;
      Curseur_A := A_Transpose.Matrice_Creuse;
      if Curseur_A = null or Curseur_B = null then
        Mat.Matrice_Creuse := null;
      else
        while Curseur_B /= null loop
          Nouvelle_Colonne :=
           new T_Colonne'
            (Colonne => Curseur_B.all.Colonne, Suivante => null,
             Vecteur => null);
          if Colonne_Prec = null then
            Mat.Matrice_Creuse := Nouvelle_Colonne;
          else
            Colonne_Prec.all.Suivante := Nouvelle_Colonne;
          end if;
          Colonne_Prec := Nouvelle_Colonne;

          while Curseur_A /= null loop
            Nouvelle_Cellule :=
             new T_Cellule'
              (Ligne    => Curseur_A.all.Colonne,
               Valeur   =>
                Produit_Scalaire
                 (Curseur_A.all.Vecteur, Curseur_B.all.Vecteur),
               Suivante => null);
            if Cellule_Prec = null then
              Colonne_Prec.all.Vecteur := Nouvelle_Cellule;
            else
              Cellule_Prec.all.Suivante := Nouvelle_Cellule;
            end if;
            Cellule_Prec := Nouvelle_Cellule;
            Curseur_A    := Curseur_A.all.Suivante;
          end loop;
          Curseur_B    := Curseur_B.all.Suivante;
          Curseur_A    := A_Transpose.Matrice_Creuse;
          Cellule_Prec := null;

        end loop;
        --    while Curseur_A.all.ligne = Curseur_B.all.Ligne
        --    loop
        --      Nouvelle_Cellule :=
        --       new T_Cellule'
        --        (Ligne   => Curseur_A.all.Ligne, Colonne => Curseur_B.all.Colonne,
        --         Valeur  => Curseur_A.all.Valeur * Curseur_B.all.Valeur,
        --         Suivant => null);

        --      if Cellule_Prec = null then
        --        Mat.Matrice_Creuse := Nouvelle_Cellule;
        --      else
        --        Cellule_Prec.all.Suivant := Nouvelle_Cellule;
        --      end if;
        --      Cellule_Prec := Nouvelle_Cellule;
        --      Curseur_B    := Curseur_B.all.Suivant;
        --    end loop;
        --    Curseur_A := Curseur_A.all.Suivant;
        --    Curseur_B := B_Transpose.Matrice_Creuse;

        --  Version Naïve

        --  for I in 1 .. Mat.Lignes loop
        --    for J in 1 .. Mat.Colonnes loop
        --      Accu := Get (A, I, 1) * Get (B, 1, J);
        --      for K in 2 .. A.Colonnes loop
        --        Accu := Accu + Get (A, I, K) * Get (B, K, J);
        --      end loop;
        --      Set (Mat, I, J, Accu);
        --    end loop;
        --  end loop;
      end if;
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
      Curseur, Colonne_Prec, Nouvelle_Colonne : T_Matrice_Creuse;
    begin
      if B = Zero then
        Mat.Matrice_Creuse := null;
      else
        Curseur := A.Matrice_Creuse;

        while Curseur /= null loop
          Nouvelle_Colonne :=
           new T_Colonne'
            (Colonne => Curseur.all.Colonne, Suivante => Curseur.all.Suivante,
             Vecteur => Multiplication_Creuse_V (Curseur.all.Vecteur, B));

          if Colonne_Prec = null then
            Mat.Matrice_Creuse := Nouvelle_Colonne;
          else
            Colonne_Prec.all.Suivante := Nouvelle_Colonne;
          end if;

          Curseur      := Curseur.all.Suivante;
          Colonne_Prec := Nouvelle_Colonne;
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
      Curseur, Colonne_Prec, Nouvelle_Colonne : T_Matrice_Creuse;
    begin
      if A = Zero then
        Mat.Matrice_Creuse := null;
      else
        Curseur := B.Matrice_Creuse;

        while Curseur /= null loop
          Nouvelle_Colonne :=
           new T_Colonne'
            (Colonne => Curseur.all.Colonne, Suivante => Curseur.all.Suivante,
             Vecteur => Multiplication_Creuse_V (Curseur.all.Vecteur, A));

          if Colonne_Prec = null then
            Mat.Matrice_Creuse := Nouvelle_Colonne;
          else
            Colonne_Prec.all.Suivante := Nouvelle_Colonne;
          end if;

          Curseur      := Curseur.all.Suivante;
          Colonne_Prec := Nouvelle_Colonne;
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
      Curseur_Colonne : T_Matrice_Creuse := Mat.Matrice_Creuse;
      Curseur_Cellule : T_Vecteur_Creux;
    begin
      if Curseur_Colonne = null then
        null;
      else
        while Curseur_Colonne /= null loop
          Curseur_Cellule := Curseur_Colonne.all.Vecteur;
          while Curseur_Cellule /= null loop
            Traiter
             (Curseur_Cellule.all.Ligne, Curseur_Colonne.all.Colonne,
              Curseur_Cellule.all.Valeur);
            Curseur_Cellule := Curseur_Cellule.all.Suivante;
          end loop;
          Curseur_Colonne := Curseur_Colonne.all.Suivante;
        end loop;
      end if;
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
      Curseur_Colonne : T_Matrice_Creuse := A.Matrice_Creuse;
      Curseur_Cellule : T_Vecteur_Creux;
    begin
      while Curseur_Colonne /= null loop
        Curseur_Cellule := Curseur_Colonne.all.Vecteur;
        while Curseur_Cellule /= null loop
          Set
           (Mat, Curseur_Colonne.all.Colonne, Curseur_Cellule.all.Ligne,
            Curseur_Cellule.all.Valeur);
          Curseur_Cellule := Curseur_Cellule.all.Suivante;
        end loop;
        Curseur_Colonne := Curseur_Colonne.all.Suivante;
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
