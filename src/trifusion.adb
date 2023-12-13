package body Trifusion is

   procedure Tri (Vecteur : in out T_Matrice; Ordre : out Matrice_Ordre.T_Matrice) is

      procedure Tri_Recursif
        (Vecteur : in out T_Matrice; Deb, Fin : in Integer)
      is

         procedure Fusion
           (Vecteur : in out T_Matrice; Deb, Milieu, Fin : in Integer)
         is
            I, J         : Integer;
            Vecteur_trie : T_Matrice := Vecteur;
            Ordre_trie   : Matrice_Ordre.T_Matrice := Ordre;
         begin
            I := 0;
            J := 0;
            while (I + Deb < Milieu) and then (J + Milieu <= Fin) loop
               if Vecteur (1, J + Milieu) < Vecteur (1, I + Deb) then
                  Vecteur_trie (1, Deb + I + J) := Vecteur (1, I + Deb);
                  Ordre_trie (1, Deb + I + J)   := Ordre (1, I + Deb);
                  I                             := I + 1;
               else
                  Vecteur_trie (1, Deb + I + J) := Vecteur (1, J + Milieu);
                  Ordre_trie (1, Deb + I + J)   := Ordre (1, J + Milieu);
                  J                             := J + 1;
               end if;
            end loop;

            -- Recopier vecteur_trie dans vecteur
            while I + Deb < Milieu loop
               Vecteur_trie (1, I + J + Deb) := Vecteur (1, I + Deb);
               Ordre_trie (1, I + J + Deb)   := Ordre (1, I + Deb);
               I                             := I + 1;
            end loop;
            while J + Milieu <= Fin loop
               Vecteur_trie (1, I + J + Deb) := Vecteur (1, J + Milieu);
               Ordre_trie (1, I + J + Deb)   := Ordre (1, J + Milieu);
               J                             := J + 1;
            end loop;
            Vecteur := Vecteur_trie;
            Ordre   := Ordre_trie;
         end Fusion;

         Milieu : Integer;

      begin
         if Deb < Fin then
            Milieu := (Deb + Fin) / 2;
            Tri_Recursif (Vecteur, Deb, Milieu);
            Tri_Recursif (Vecteur, Milieu + 1, Fin);
            Fusion (Vecteur, Deb, Milieu + 1, Fin);
         end if;
      end Tri_Recursif;

   begin
      Tri_Recursif (Vecteur, 1, Vecteur'Length (2));
   end Tri;

end Trifusion;
