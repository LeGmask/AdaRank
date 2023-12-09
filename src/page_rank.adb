with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Matrice;

procedure Page_Rank is

   procedure M_Plein
     (Reseau  : Unbounded_String; Alpha : Float; k : Integer; Eps : Float;
      Prefixe : Unbounded_String)
   is
      package T_Matrice_Float is new Matrice
        (Float, Standard."+", Standard."*");
      use T_Matrice_Float;

      File : Ada.Text_IO.File_Type;

      I : Integer;
      J : Integer;
      N : Integer;

   begin
      -- Charger le graphe dans une matrice d'adjacence pondérée
      Open (File, In_File, To_String (Reseau));
      Get (File, N);

      --! Stocker dans H chaque référencement
      while not End_Of_File (File) loop
         Get (File, I);
         Get (File, J);

      end loop;
      Close (File);
      --! Pondérer les lignes

   end M_Plein;

   ALPHA_INVALIDE       : exception;
   EPS_INVALIDE         : exception;
   K_INVALIDE           : exception;
   PAS_DE_FICHIER       : exception;
   EMPLACEMENT_INVALIDE : exception;
   SYNTAXE              : exception;

   Reseau  : Unbounded_String;
   Alpha   : Float            := 0.85;
   K       : Integer          := 150;
   Eps     : Float            := 0.0;
   Prefixe : Unbounded_String := To_Unbounded_String ("output");
   Plein   : Boolean          := True;

   I : Integer := 1;

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
            when 'p' =>
               Plein := True;
               I     := I + 1;
            when 'c' =>
               Plein := False;
               I     := I + 1;
            when 'a' =>
               Alpha := Float'Value (Argument (I + 1));
               if Alpha > 1.0 or else Alpha < 0.0 then
                  raise ALPHA_INVALIDE;
               end if;
               I := I + 2;
            when 'r' =>
               Eps := Float'Value (Argument (I + 1));
               if Eps < 0.0 then
                  raise EPS_INVALIDE;
               end if;
               I := I + 2;
            when 'r' =>
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
      M_Plein (Reseau, Alpha, k, Eps, Prefixe);
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
