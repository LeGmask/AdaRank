with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Matrice;
with Trifusion;
with Export;

procedure Page_Rank is
   PRECISION : constant := 14;
   type T_Double is digits PRECISION;

   package Double_IO is new Ada.Text_IO.Float_IO (T_Double);
   use Double_IO;

   package Matrice_Float is new Matrice
     (T_Double, 0.0, 1.0, "+", "-", "*", "/");
   use Matrice_Float;
   package Matrice_Integer is new Matrice (Integer, 0, 1, "+", "-", "*", "/");

   package Tri_Fusion is new Trifusion (Matrice_Float, Matrice_Integer, "<");

   package Export_PageRank is new Export
     (Matrice_Pi => Matrice_Float, Matrice_Ordre => Matrice_Integer);
   use Export_PageRank;

   procedure Put_Element (Fichier : File_Type; E : T_Double) is
   begin
      Put (File => Fichier, Item => E, Fore => 0, Exp => 0);
   end Put_Element;
   procedure Put_Element (Fichier : File_Type; E : Integer) is
   begin
      Put (Fichier, E, 1);
   end Put_Element;

   procedure Sauver is new Export_Resultats (Put_Element, Put_Element);

   procedure Algorithme
     (Alpha : in     Float; K : in Integer; Eps : in Float; N : in Integer;
      Plein : in     Boolean; H : in T_Matrice; Pi : out T_Matrice;
      Ordre :    out Matrice_Integer.T_Matrice)
   is

      function Norme (A : in T_Matrice) return T_Double is
         Max_Abs : T_Double := abs (Get (A, 1, 1));
         Element : T_Double;
      begin
         for I in 1 .. A.Lignes loop
            for J in 1 .. A.Colonnes loop
               Element := abs (Get (A, I, J));
               if Element > Max_Abs then
                  Max_Abs := Element;
               end if;
            end loop;
         end loop;
         return Max_Abs;
      end Norme;

      I : Integer;

      G        : T_Matrice (N, N, Plein);
      Pi_avant : T_Matrice (1, N, True);
      Pi_norm  : T_Matrice (1, N, True);

   begin
      -- Appliquer l'algorithme PageRank jusqu'à terminaison
      if Plein then
         declare
            Attila : T_Matrice (N, N, True);
            S      : T_Matrice (N, N, True);
         begin
            --! Créer la matrice S en pondérant
            S := Copie (H);
            for I in 1 .. N loop
               if Get_Poids (S, I) = 0.0 then
                  for J in 1 .. N loop
                     Set (S, I, J, T_Double (1.0 / Float (N)));
                     Set_Poids (S, I, T_Double (N));
                  end loop;
               else
                  for J in 1 .. N loop
                     Set
                       (S, I, J,
                        T_Double
                          (Float (Get (S, I, J)) / Float (Get_Poids (S, I))));
                  end loop;
               end if;
            end loop;

            --! Créer la matrice G
            Init (Attila, 1.0);
            G :=
              T_Double (Alpha) * S +
              T_Double ((1.0 - Alpha) / Float (N)) * Attila;
         end;
      else
         G := H;
      end if;

      --! Calculer la matrice Pi par itérations
      I := 0;

      Init (Pi, T_Double (1.0 / Float (N)));
      Pi_avant := Pi;
      PageRankIter (Pi, G, T_Double (Alpha), T_Double (N));

      -- Calcul du Pi utilisé pour la norme
      Pi_norm := Pi + (Pi_avant * (-1.0));

      while (I < K) and then Norme (Pi_norm) > T_Double (Eps) loop
         Pi_avant := Pi;

         PageRankIter (Pi, G, T_Double (Alpha), T_Double (N));

         -- Calcul du Pi utilisé pour la norme
         Pi_norm := Pi + Pi_avant * (-1.0);

         I := I + 1;
      end loop;

      for I in 1 .. N loop
         Matrice_Integer.Set (Ordre, 1, I, I - 1);
      end loop;

      Tri_Fusion.Tri (Pi, Ordre);

      Detruire (G);
   end Algorithme;

   ALPHA_INVALIDE : exception;
   EPS_INVALIDE   : exception;
   K_INVALIDE     : exception;
   PAS_DE_FICHIER : exception;
   SYNTAXE        : exception;

   File : Ada.Text_IO.File_Type;

   Reseau  : Unbounded_String;
   Alpha   : Float            := 0.85;
   K       : Integer          := 150;
   Eps     : Float            := 0.0;
   Prefixe : Unbounded_String := To_Unbounded_String ("output");
   Plein   : Boolean          := True;

   I : Integer := 1;
   N : Integer;

begin
   -- Gérer les arguments avec lesquels le programme est lancé

   --! Récupérer les arguments fournis
   if not (Argument_Count > 0) then
      raise PAS_DE_FICHIER;
   else
      Reseau := To_Unbounded_String (Argument (Argument_Count));
   end if;

   while I < Argument_Count loop
      if Argument (I) (1) = '-'
        and then Length (To_Unbounded_String (Argument (I))) = 2
      then
         case Argument (I) (2) is
            when 'P' =>
               Plein := True;
               I     := I + 1;
            when 'C' =>
               Plein := False;
               I     := I + 1;
            when 'A' =>
               begin
                  Alpha := Float'Value (Argument (I + 1));
                  if Alpha > 1.0 or else Alpha < 0.0 then
                     raise ALPHA_INVALIDE;
                  end if;
                  I := I + 2;
               exception
                  when Constraint_Error =>
                     raise ALPHA_INVALIDE;
               end;
            when 'E' =>
               begin
                  Eps := Float'Value (Argument (I + 1));
                  if Eps < 0.0 then
                     raise EPS_INVALIDE;
                  end if;
                  I := I + 2;
               exception
                  when Constraint_Error =>
                     raise EPS_INVALIDE;
               end;
            when 'K' =>
               begin
                  K := Integer'Value (Argument (I + 1));
                  if K < 0 then
                     raise K_INVALIDE;
                  end if;
                  I := I + 2;
               exception
                  when Constraint_Error =>
                     raise K_INVALIDE;
               end;
            when 'R' =>
               Prefixe := To_Unbounded_String (Argument (I + 1));
               I       := I + 2;
            when others =>
               raise SYNTAXE;
         end case;
      else
         raise SYNTAXE;
      end if;
   end loop;
   Open (File, In_File, To_String (Reseau));
   Get (File, N);

   declare
      H     : T_Matrice (N, N, Plein);
      Pi    : T_Matrice (1, N, True);
      Ordre : Matrice_Integer.T_Matrice (1, N, True);
   begin
      Init_Fichier (File, H);
      Close (File);
      Algorithme (Alpha, K, Eps, N, Plein, H, Pi, Ordre);
      Sauver (Pi, Ordre, N, K, Alpha, To_String (Prefixe));
      Detruire (Pi);
      Matrice_Integer.Detruire (Ordre);
   end;

   --! Vérifier si les arguments sont valides
exception
   when PAS_DE_FICHIER =>
      Put_Line ("Pas de fichier fourni, veuillez relire votre appel");
   when ALPHA_INVALIDE =>
      Put_Line ("Alpha invalide, veuillez relire votre appel");
   when EPS_INVALIDE   =>
      Put_Line ("Epsilon invalide, veuillez relire votre appel");
   when K_INVALIDE     =>
      Put_Line ("K invalide, veuillez relire votre appel");
   when SYNTAXE        =>
      Put_Line ("Syntaxe invalide, veuillez relire votre appel");
   when Name_Error     =>
      Put_Line
        ("Emplacement du fichier invalide, veuillez relire votre appel");
   -- when others               =>
   --   null;
end Page_Rank;
