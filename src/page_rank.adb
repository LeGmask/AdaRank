with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Matrice;
with Graphe;
with Trifusion;
with Export;

procedure Page_Rank is
   package Matrice_Float is new Matrice
     (Float, 0.0, Standard."+", Standard."*");
   use Matrice_Float;
   package Matrice_Integer is new Matrice
     (Integer, 0, Standard."+", Standard."*");

   package Graphe_Float is new Graphe (Matrice_Float, 1.0, "/");
   use Graphe_Float;
   package Tri_Fusion is new Trifusion (Matrice_Float, Matrice_Integer, "<");

   package Export_PageRank is new Export
     (Matrice_Pi => Matrice_Float, Matrice_Ordre => Matrice_Integer);
   use Export_PageRank;

   procedure Put_Element (Fichier : File_Type; E : Float) is
   begin
      Put (Fichier, E, 1);
   end Put_Element;
   procedure Put_Element (Fichier : File_Type; E : Integer) is
   begin
      Put (Fichier, E, 1);
   end Put_Element;

   procedure Sauver is new Export_Resultats (Put_Element, Put_Element);

   procedure Algorithme
     (Alpha : in     Float; K : in Integer; Eps : in Float; N : in Integer;
      Plein : in Boolean; H, Sortants : in out T_Matrice; Pi : out T_Matrice;
      Ordre :    out Matrice_Integer.T_Matrice)
   is

      function Norme (A : in T_Matrice) return Float is
         Max_Abs : Float := abs (Get (A, 1, 1));
      begin
         for I in 1 .. A.Lignes loop
            for J in 1 .. A.Colonnes loop
               if abs (Get (A, I, J)) > Max_Abs then
                  Max_Abs := abs (Get (A, I, J));
               end if;
            end loop;
         end loop;
         return Max_Abs;
      end Norme;

      I : Integer;

      S           : T_Matrice (N, N, Plein);
      G           : T_Matrice (N, N, Plein);
      Attila      : T_Matrice (N, N, Plein);
      Pi_avant    : T_Matrice (1, N, True);
      Pi_detruire : T_Matrice (1, N, True);
      Pi_norm     : T_Matrice (1, N, True);

      --  Ordre    : Matrice_Integer.T_Matrice (1, N, True);

   begin
      -- Appliquer l'algorithme PageRank jusqu'à terminaison
      if Plein then
         --! Créer la matrice S
         S := H;
         for I in 1 .. N loop
            if Get (Sortants, I, 1) = 0.0 then
               for J in 1 .. N loop
                  Set (S, I, J, 1.0 / Float (N));
               end loop;
            end if;
         end loop;

         --! Créer la matrice G
         Init (Attila, 1.0);
         G := Alpha * S + ((1.0 - Alpha) / Float (N)) * Attila;
      else
         G := Copie (H);
      end if;

      --! Calculer la matrice Pi par itérations
      I := 0;
      Init (Pi_avant, 1.0 / Float (N));
      Pi := Pi_avant * G;

      -- Calcul du Pi utilisé pour la norme
      Pi_detruire := Pi_avant * (-1.0);
      Pi_norm     := Pi + Pi_detruire;
      Detruire (Pi_detruire);

      while (I < K) and then Norme (Pi_norm) > Eps loop
         Detruire (Pi_avant);
         Pi_avant := Copie (Pi);

         Pi_detruire := Pi;
         Pi          := Pi * G;
         Detruire (Pi_detruire);

         -- Calcul du Pi utilisé pour la norme
         Detruire (Pi_norm); -- On détruit l'ancien Pi_norm
         Pi_detruire := Pi_avant * (-1.0);
         Pi_norm     := Pi + Pi_detruire;
         Detruire (Pi_detruire);

         I := I + 1;
      end loop;
      Detruire (Pi_norm);

      for I in 1 .. N loop
         Matrice_Integer.Set (Ordre, 1, I, I - 1);
      end loop;
      Tri_Fusion.Tri (Pi, Ordre);

      Detruire (S);
      Detruire (G);
      Detruire (Pi_avant);
      Detruire (Attila);
      Detruire (H);
      Detruire (Sortants);
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
               Alpha := Float'Value (Argument (I + 1));
               if Alpha > 1.0 or else Alpha < 0.0 then
                  raise ALPHA_INVALIDE;
               end if;
               I := I + 2;
            when 'E' =>
               Eps := Float'Value (Argument (I + 1));
               if Eps < 0.0 then
                  raise EPS_INVALIDE;
               end if;
               I := I + 2;
            when 'K' =>
               K := Integer'Value (Argument (I + 1));
               if K < 0 then
                  raise K_INVALIDE;
               end if;
               I := I + 2;
            when 'r' =>
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
      H        : T_Matrice (N, N, Plein);
      Sortants : T_Matrice (N, 1, True);
      Pi       : T_Matrice (1, N, True);
      Ordre    : Matrice_Integer.T_Matrice (1, N, True);
   begin
      Lire_Graphe (File, H, Sortants);
      Close (File);
      Ponderer_Graphe (H, Sortants);
      Algorithme (Alpha, K, Eps, N, Plein, H, Sortants, Pi, Ordre);
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
