--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Matrice is

  procedure Free is new Ada.Unchecked_Deallocation
   (Object => T_Cellule, Name => T_Matrice_Creuse);

  procedure Init (Mat : out T_Matrice; Val : in T_Valeur := Zero) is

    procedure Init_Pleine is
    begin
      Mat.Matrice_Pleine := (others => (others => Val));
    end Init_Pleine;

    procedure Init_Creuse is
      Curseur, Nouvelle_Cellule : T_Matrice_Creuse;
      Index                     : Natural := 1;
    begin
      if Val = Zero then
        Mat.Matrice_Creuse := null;
      else
        Curseur :=
         new T_Cellule'
          (Ligne => 1, Colonne => 1, Valeur => Val, Suivant => null);

        Mat.Matrice_Creuse := Curseur;

        while Index < Mat.Lignes * Mat.Colonnes loop
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne   => (Index / (Mat.Colonnes)) + 1,
             Colonne => (Index mod Mat.Colonnes) + 1, Valeur => Val,
             Suivant => null);

          Curseur.all.Suivant := Nouvelle_Cellule;
          Curseur             := Nouvelle_Cellule;
          Index               := Index + 1;
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
    Copie: T_Matrice(Mat.Lignes, Mat.Colonnes, Mat.Pleine);

    procedure Copier_Pleine is
    begin
      Copie.Matrice_Pleine := Mat.Matrice_Pleine;
    end Copier_Pleine;

    procedure Copier_Creuse is
      Curseur, Cellule_Prec, Nouvelle_Cellule : T_Matrice_Creuse;
    begin
      Curseur := Mat.Matrice_Creuse;

      while Curseur /= null loop
        Nouvelle_Cellule :=
         new T_Cellule'
          (Ligne   => Curseur.all.Ligne, Colonne => Curseur.all.Colonne,
           Valeur  => Curseur.all.Valeur, Suivant => null);

        if Cellule_Prec = null then
          Copie.Matrice_Creuse := Nouvelle_Cellule;
        else
          Cellule_Prec.all.Suivant := Nouvelle_Cellule;
        end if;

        Curseur := Curseur.all.Suivant;
        Cellule_Prec := Nouvelle_Cellule;
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
  begin
    if Mat.Pleine then
      null;
    else
      declare
        Curseur, Suivant : T_Matrice_Creuse;
      begin
        Curseur := Mat.Matrice_Creuse;

        while Curseur /= null loop
          Suivant := Curseur.all.Suivant;
          Free (Curseur);
          Curseur := Suivant;
        end loop;
      end;
    end if;
  end Detruire;

  procedure TrouverParRecursion
   (Matrice : in     T_Matrice_Creuse; Ligne, Colonne : in Positive;
    Cellule :    out T_Matrice_Creuse)
  is
  begin
    if Matrice = null then
      Cellule := null;
    elsif Matrice.all.Ligne = Ligne and then Matrice.all.Colonne = Colonne then
      Cellule := Matrice;
    elsif Matrice.all.Ligne > Ligne or
     (Matrice.all.Ligne = Ligne and Matrice.all.Colonne > Colonne)
    then
      Cellule := null;
    else
      TrouverParRecursion (Matrice.all.Suivant, Ligne, Colonne, Cellule);
    end if;
  end TrouverParRecursion;

  function Get (Mat : in T_Matrice; Ligne, Colonne : Positive) return T_Valeur
  is
  begin
    if Mat.Pleine then
      return Mat.Matrice_Pleine (Ligne, Colonne);
    else
      declare
        Cellule : T_Matrice_Creuse;
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
    Cellule, Cellule_Prec, Cellule_Suiv :    out T_Matrice_Creuse)
  is
  begin
    if Matrice = null then
      Cellule := null;
    elsif Matrice.all.Ligne = Ligne and Matrice.all.Colonne = Colonne then
      Cellule := Matrice;
    elsif Matrice.all.Ligne > Ligne or
     (Matrice.all.Ligne = Ligne and Matrice.all.Colonne > Colonne)
    then
      Cellule      := null;
      Cellule_Suiv := Matrice;
    else
      Cellule_Prec := Matrice;
      Cellule_Suiv := Matrice.all.Suivant;
      TrouverParRecursion
       (Matrice.all.Suivant, Ligne, Colonne, Cellule, Cellule_Prec,
        Cellule_Suiv);
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
      Cellule, Cellule_Prec, Cellule_Suiv, Nouvelle_Cellule : T_Matrice_Creuse;
    begin
      TrouverParRecursion
       (Mat.Matrice_Creuse, Ligne, Colonne, Cellule, Cellule_Prec,
        Cellule_Suiv);

      if Val = Zero then
        if Cellule /= null then
          if Cellule_Prec = null then
            Mat.Matrice_Creuse := Cellule.all.Suivant;
          else
            Cellule_Prec.all.Suivant := Cellule.all.Suivant;
          end if;
          Free (Cellule);
        end if;
      else
        if Cellule = null then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne   => Ligne, Colonne => Colonne, Valeur => Val,
             Suivant => Cellule_Suiv);

          if Cellule_Prec = null then
            Mat.Matrice_Creuse := Nouvelle_Cellule;
          else
            Cellule_Prec.all.Suivant := Nouvelle_Cellule;
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

    procedure Addition_Creuse is
      Curseur_A, Curseur_B, Cellule_Prec, Nouvelle_Cellule : T_Matrice_Creuse;
    begin
      Curseur_A := A.Matrice_Creuse;
      Curseur_B := B.Matrice_Creuse;

      while Curseur_A /= null or Curseur_B /= null loop
        if Curseur_A = null and Curseur_B /= null then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne  => Curseur_B.all.Ligne, Colonne => Curseur_B.all.Colonne,
             Valeur => Curseur_B.all.Valeur, Suivant => null);

          Curseur_B := Curseur_B.all.Suivant;
        elsif Curseur_A /= null and Curseur_B = null then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne  => Curseur_A.all.Ligne, Colonne => Curseur_A.all.Colonne,
             Valeur => Curseur_A.all.Valeur, Suivant => null);

          Curseur_A := Curseur_A.all.Suivant;

        elsif Curseur_A.all.Ligne < Curseur_B.all.Ligne or
         (Curseur_A.all.Ligne = Curseur_B.all.Ligne and
          Curseur_A.all.Colonne < Curseur_B.all.Colonne)
        then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne  => Curseur_A.all.Ligne, Colonne => Curseur_A.all.Colonne,
             Valeur => Curseur_A.all.Valeur, Suivant => null);

          Curseur_A := Curseur_A.all.Suivant;
        elsif Curseur_A.all.Ligne > Curseur_B.all.Ligne or
         (Curseur_A.all.Ligne = Curseur_B.all.Ligne and
          Curseur_A.all.Colonne > Curseur_B.all.Colonne)
        then
          Nouvelle_Cellule :=
           new T_Cellule'
            (Ligne  => Curseur_B.all.Ligne, Colonne => Curseur_B.all.Colonne,
             Valeur => Curseur_B.all.Valeur, Suivant => null);

          Curseur_B := Curseur_B.all.Suivant;
        else
          if Curseur_A.all.Valeur + Curseur_B.all.Valeur = Zero then
            null;
          else
            Nouvelle_Cellule :=
             new T_Cellule'
              (Ligne => Curseur_A.all.Ligne, Colonne => Curseur_A.all.Colonne,
               Valeur  => Curseur_A.all.Valeur + Curseur_B.all.Valeur,
               Suivant => null);

          end if;

          Curseur_A := Curseur_A.all.Suivant;
          Curseur_B := Curseur_B.all.Suivant;
        end if;

        if Nouvelle_Cellule /= null then
          if Cellule_Prec = null then
            Mat.Matrice_Creuse := Nouvelle_Cellule;
          else
            Cellule_Prec.all.Suivant := Nouvelle_Cellule;
          end if;
          Cellule_Prec := Nouvelle_Cellule;
        end if;
        Nouvelle_Cellule := null;
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

    procedure Multiplication_Creuse is
    --  Curseur_A, Curseur_B, Cellule_Prec, Nouvelle_Cellule : T_Matrice_Creuse;
    --  B_Transpose : T_Matrice (B.Lignes, B.Colonnes, B.Pleine) :=
    --   Transpose (B);
    begin
      --  Curseur_A := A.Matrice_Creuse;
      --  Curseur_B := B_Transpose.Matrice_Creuse;

      --  while Curseur_A /= null and Curseur_B /= null loop
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
      --  end loop;

      for I in 1 .. Mat.Lignes loop
        for J in 1 .. Mat.Colonnes loop
          Set (Mat, I, J, Get (A, I, 1) * Get (B, 1, J));
          for K in 2 .. A.Colonnes loop
            Set (Mat, I, J, Get (Mat, I, J) + Get (A, I, K) * Get (B, K, J));
          end loop;
        end loop;
      end loop;

    end Multiplication_Creuse;
  begin
    if Mat.Pleine then
      Multiplication_Pleine;
    else
      Multiplication_Creuse;
    end if;

    return Mat;
  end "*";

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
      Curseur : T_Matrice_Creuse := A.Matrice_Creuse;
    begin
      while Curseur /= null loop
        Set
         (Mat, Curseur.all.Ligne, Curseur.all.Colonne, Curseur.all.Valeur * B);
        Curseur := Curseur.all.Suivant;
      end loop;
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
      Curseur : T_Matrice_Creuse := B.Matrice_Creuse;
    begin
      while Curseur /= null loop
        Set
         (Mat, Curseur.all.Ligne, Curseur.all.Colonne, Curseur.all.Valeur * A);
        Curseur := Curseur.all.Suivant;
      end loop;
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
      Curseur : T_Matrice_Creuse := Mat.Matrice_Creuse;
    begin
      if Curseur = null then
        null;
      else
        while Curseur /= null loop
          Traiter (Curseur.all.Ligne, Curseur.all.Colonne, Curseur.all.Valeur);
          Curseur := Curseur.all.Suivant;
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
      Curseur : T_Matrice_Creuse := A.Matrice_Creuse;
    begin
      while Curseur /= null loop
        Set (Mat, Curseur.all.Colonne, Curseur.all.Ligne, Curseur.all.Valeur);
        Curseur := Curseur.all.Suivant;
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
