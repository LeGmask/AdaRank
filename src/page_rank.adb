with Ada.Text_IO;			                use Ada.Text_IO;
with Ada.Float_Text_IO;	                use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;	             use Ada.Integer_Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Directories;
with Matrice;

procedure Page_Rank is

   procedure M_Plein
     (Reseau  : Unbounded_String; Alpha : Float; k : Integer; Eps : Float;
      Prefixe : Unbounded_String; N : Integer)
   is
      package Matrice_Float is new Matrice
        (Float, Standard."+", Standard."*");
      use Matrice_Float;

      procedure Put_Float(F : Float) is
      begin
         Put(F);
      end Put_Float;

      procedure Afficher_Matrice is new Afficher(Put_Float);

      function Norme_Frobenius(A: in T_Matrice) return Float is
         Norme : Float := 0.0;
      begin
         for I in A'Range(1) loop
            for J in A'Range(2) loop
            Norme := Norme + A(I,J) * A(I,J);
            end loop;
         end loop;
         return Sqrt(Norme);
      end Norme_Frobenius;

      File : Ada.Text_IO.File_Type;

      I : Integer;
      J : Integer;

      H:          T_Matrice(1..N, 1..N);
      Sortants :  T_Matrice(1..N, 1..1);
      S :         T_Matrice(1..N, 1..N);
      G:          T_Matrice(1..N, 1..N);
      Attila:     T_Matrice(1..N, 1..N);
      Pi_avant :  T_Matrice(1..1, 1..N);
      Pi:         T_Matrice(1..1, 1..N);

   begin
      -- Charger le graphe dans une matrice d'adjacence pondérée
      Open (File, In_File, To_String (Reseau));
      Get (File, I); -- Relecture de N déjà récupéré
      Init (H, 0.0);

      --! Stocker dans H chaque référencement
      Init (Sortants, 0.0);
      while not End_Of_File (File) loop
         Get (File, I);
         Get (File, J);
         H (I+1, J+1) := H (I+1, J+1) + 1.0;
         Sortants (I+1, 1) := Sortants (I+1, 1) + 1.0;
      end loop;
      Close (File);

      --! Pondérer les lignes
      for I in 1..N loop
         for J in 1..N loop
            if Sortants (I,1) = 0.0 then
               H (I,J) := 0.0;
            else
               H (I,J) := H (I,J) / Sortants (I,1);
            end if;
         end loop;
      end loop;

      -- Appliquer l'algorithme PageRank jusqu'à terminaison

      --! Créer la matrice S
      S := H;
      for I in 1..N loop
         if Sortants (I,1) = 0.0 then
            for J in 1..N loop
               S(I,J) := 1.0 / float (N);
            end loop;
         end if;
      end loop;

      --! Créer la matrice G
      Init (Attila, 1.0);
      G := Alpha * S + ( (1.0 - Alpha) / float (N)) * Attila;

      --! Calculer la matrice Pi par itérations
      I := 0;
      Init (Pi_avant, 1.0 / float(N));
      Pi := Pi_avant * G;
      while (I < k) 
        and then Norme_Frobenius( Pi + (Pi_avant * (-1.0)) ) > Eps loop
         Pi_avant := Pi;
         Pi := Pi * G;
         I := I + 1;
      end loop;

      Afficher_Matrice(Pi);
   end M_Plein;

   ALPHA_INVALIDE       : exception;
   EPS_INVALIDE         : exception;
   K_INVALIDE           : exception;
   PAS_DE_FICHIER       : exception;
   EMPLACEMENT_INVALIDE : exception;
   SYNTAXE              : exception;

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
      if not (Ada.Directories.Exists (To_String (Reseau))) then
         raise EMPLACEMENT_INVALIDE;
      end if;
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
               k := Integer'Value (Argument (I + 1));
               if k < 0 then
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
   
   if Plein then
      Open (File, In_File, To_String(Reseau));
      Get (File, N);
      Close (File);
      M_Plein (Reseau, Alpha, k, Eps, Prefixe, N);
   else
      null;
   end if;

--! Vérifier si les arguments sont valides
exception
   when PAS_DE_FICHIER       =>
      Put_Line ("Pas de fichier fourni, veuillez relire votre appel");
   when ALPHA_INVALIDE       =>
      Put_Line ("Alpha invalide, veuillez relire votre appel");
   when EPS_INVALIDE         =>
      Put_Line ("Epsilon invalide, veuillez relire votre appel");
   when K_INVALIDE           =>
      Put_Line ("K invalide, veuillez relire votre appel");
   when SYNTAXE              =>
      Put_Line ("Syntaxe invalide, veuillez relire votre appel");
   when EMPLACEMENT_INVALIDE =>
      Put_Line
        ("Emplacement du fichier invalide, veuillez relire votre appel");
   when others               =>
      null;

end Page_Rank;
