with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;
with Ada.Command_line;		use Ada.Command_line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with matrice;

procedure Page_Rank is

   procedure M_Plein(reseau: Unbounded_String;alpha : Float;k : Integer;eps : Float; prefixe:Unbounded_String) is
      package T_Matrice_Float is new matrice(Float,Standard."+",Standard."*");
      use T_Matrice_Float;

      File : Ada.Text_IO.File_Type;

      i : Integer;
      j : Integer;
      n : Integer;


   begin
      -- Charger le graphe dans une matrice d'adjacence pondérée
      open (File, In_File, To_String(reseau));
      Get (File, n);
      

      --! Stocker dans H chaque référencement
      while not End_Of_file (File) loop
         Get (File, i);
         Get (File, j);

      end loop;
      Close (File);
      --! Pondérer les lignes

   end M_Plein;

   ALPHA_INVALIDE : exception;
   EPS_INVALIDE : exception;
   K_INVALIDE : exception;
   PAS_DE_FICHIER : exception;
   EMPLACEMENT_INVALIDE : exception;
   SYNTAXE : exception;


   reseau : Unbounded_String;
   alpha : Float := 0.85;
   k : Integer := 150;
   eps : Float := 0.0;
   prefixe : Unbounded_String := To_Unbounded_String("output");
   plein : Boolean := True;

   i : Integer := 1;

begin
   -- Gérer les arguments avec lesquels le programme est lancé

   --! Récupérer les arguments fournis
   if not(Argument_Count>0) then
      raise PAS_DE_FICHIER;
   else
      reseau := To_Unbounded_String(Argument(Argument_Count));
      if not(Ada.Directories.Exists(To_String(reseau))) then
         raise EMPLACEMENT_INVALIDE;
      end if;
   end if;

   while i<(Argument_Count) loop
      if Argument(i)(1)='-' and then Length(To_Unbounded_String(Argument(i)))=2 then
         case Argument(i)(2) is
         when 'P' => plein:=True;
            i:=i+1;
         when 'C' => plein:=False;
            i:=i+1;
         when 'A' => alpha := float'Value(Argument(i+1));
            if alpha>1.0 or else alpha<0.0 then
               raise ALPHA_INVALIDE;
            end if;
            i:=i+2;
         when 'E' => eps := float'Value(Argument(i+1));
            if eps<0.0 then
               raise EPS_INVALIDE;
            end if;
            i:=i+2;
         when 'K' => k := integer'Value(Argument(i+1));
            if k<0 then
               raise K_INVALIDE;
            end if;
            i:=i+2;
         when 'R' => prefixe := To_Unbounded_String(Argument(i+1));
            i:=i+2;
         when others => raise SYNTAXE;
         end case;
      else
         raise SYNTAXE;
      end if;
   end loop;
   
   if plein then
      M_Plein(reseau,alpha,k,eps,prefixe);
   else
      null;
   end if;

   --! Vérifier si les arguments sont valides
   exception
   when PAS_DE_FICHIER =>
      Put_Line("Pas de fichier fourni, veuillez relire votre appel");
   when ALPHA_INVALIDE =>
      Put_Line("Alpha invalide, veuillez relire votre appel");
   when EPS_INVALIDE =>
      Put_Line("Epsilon invalide, veuillez relire votre appel");
   when K_INVALIDE =>
      Put_Line("K invalide, veuillez relire votre appel");
   when SYNTAXE =>
      Put_Line("Syntaxe invalide, veuillez relire votre appel");
   when EMPLACEMENT_INVALIDE =>
      Put_Line("Emplacement du fichier invalide, veuillez relire votre appel");
   when others => null;


end Page_Rank;

