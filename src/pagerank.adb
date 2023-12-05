with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Float_Text_IO;		use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;
with Ada.Command_line;		use Ada.Command_line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;

procedure PageRank is


   File : Ada.Text_IO.File_Type;

   reseau : Unbounded_String;
   alpha : Float := 0.85;
   k : Integer := 150;
   eps : Float := 0.0;
   prefixe : Unbounded_String := To_Unbounded_String("output");
   plein : Boolean := True;
   valide : Boolean := True;
   i : Integer := 1;

begin
   --! Gérer les arguments avec lesquels le programme est lancé

   --! Récupérer les arguments fournis
   while i<(Argument_Count) loop
      if Argument(i)(1)='-' and then Length(To_Unbounded_String(Argument(i)))=2 then
         case Argument(i)(2) is
         when 'P' => plein:=True;
            i:=i+1;
         when 'C' => plein:=False;
            i:=i+1;
         when 'A' => alpha := float'Value(Argument(i+1));
            i:=i+2;
         when 'E' => eps := float'Value(Argument(i+1));
            i:=i+2;
         when 'K' => k := integer'Value(Argument(i+1));
            i:=i+2;
         when 'R' =>prefixe := To_Unbounded_String(Argument(i+1));
            i:=i+2;
         when others => valide:= False;
         end case;
      else
         valide:=False;
         i:= i+1;
      end if;
   end loop;
   reseau := To_Unbounded_String(Argument(i));

   --! Vérifier si les arguments sont valides
   if alpha>1.0 or else alpha<0.0 or else k<0 or else eps<0.0 or else not(valide) or else not(Ada.Directories.Exists(To_String(reseau))) then
      Put_Line("Un argument a une valeur illégale, veuillez relire votre appel");
      valide := False;
   end if;





end PageRank;

