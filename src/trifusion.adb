package body Trifusion is

   procedure Tri
     (Vecteur : in out T_Matrice; Ordre : out Matrice_Ordre.T_Matrice)
   is

      procedure Tri_Recursif
        (Vecteur : in out T_Matrice; Deb, Fin : in Integer)
      is

         procedure Fusion
           (Vecteur : in out T_Matrice; Deb, Milieu, Fin : in Integer)
         is
            I, J               : Integer                 := 0;
            Vecteur_trie       : T_Matrice               := Vecteur;
            Ordre_trie         : Matrice_Ordre.T_Matrice := Ordre;
            Temp1, Temp2       : T_Valeur;
         begin
            while (I + Deb < Milieu) and then (J + Milieu <= Fin) loop
               Temp1 := Get (Vecteur, 1, I + Deb);
               Temp2 := Get (Vecteur, 1, J + Milieu);
               if Temp2 < Temp1 then
                  Set (Vecteur_trie, 1, Deb + I + J, Temp1);
                  Matrice_Ordre.Set
                    (Ordre_trie, 1, Deb + I + J,
                     Matrice_Ordre.Get (Ordre, 1, I + Deb));
                  I := I + 1;
               else
                  Set (Vecteur_trie, 1, Deb + I + J, Temp2);
                  Matrice_Ordre.Set
                    (Ordre_trie, 1, Deb + I + J,
                     Matrice_Ordre.Get (Ordre, 1, J + Milieu));
                  J := J + 1;
               end if;
            end loop;

            -- Recopier vecteur_trie dans vecteur
            while I + Deb < Milieu loop
               Set (Vecteur_trie, 1, I + J + Deb, Get (Vecteur, 1, I + Deb));
               Matrice_Ordre.Set
                 (Ordre_trie, 1, I + J + Deb,
                  Matrice_Ordre.Get (Ordre, 1, I + Deb));
               I := I + 1;
            end loop;
            while J + Milieu <= Fin loop
               Set
                 (Vecteur_trie, 1, I + J + Deb, Get (Vecteur, 1, J + Milieu));
               Matrice_Ordre.Set
                 (Ordre_trie, 1, I + J + Deb,
                  Matrice_Ordre.Get (Ordre, 1, J + Milieu));
               J := J + 1;
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
      Tri_Recursif (Vecteur, 1, Vecteur.Colonnes);
   end Tri;

end Trifusion;
