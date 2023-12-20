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
            I, J               : Integer;
            Vecteur_trie       : T_Matrice               := Copie (Vecteur);
            Ordre_trie         : Matrice_Ordre.T_Matrice :=
              Matrice_Ordre.Copie (Ordre);
            Vecteur_A_Detruire : T_Matrice               := Vecteur;
            Ordre_A_Detruire   : Matrice_Ordre.T_Matrice := Ordre;
         begin
            I := 0;
            J := 0;
            while (I + Deb < Milieu) and then (J + Milieu <= Fin) loop
               if Get (Vecteur, 1, J + Milieu) < Get (Vecteur, 1, I + Deb) then
                  Set
                    (Vecteur_trie, 1, Deb + I + J, Get (Vecteur, 1, I + Deb));
                  Matrice_Ordre.Set
                    (Ordre_trie, 1, Deb + I + J,
                     Matrice_Ordre.Get (Ordre, 1, I + Deb));
                  I := I + 1;
               else
                  Set
                    (Vecteur_trie, 1, Deb + I + J,
                     Get (Vecteur, 1, J + Milieu));
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
            Vecteur := Copie (Vecteur_trie);
            Ordre   := Matrice_Ordre.Copie (Ordre_trie);
            Detruire (Vecteur_A_Detruire);
            Matrice_Ordre.Detruire (Ordre_A_Detruire);
            Matrice_Ordre.Detruire (Ordre_trie);
            Detruire (Vecteur_trie);
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
