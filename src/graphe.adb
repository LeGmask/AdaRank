with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Graphe is
    procedure Lire_Graphe (File : in File_Type; Adj, Sortants : out T_Matrice)
    is
        I, J : Integer;
    begin
        Init (Adj);
        Init (Sortants);

        -- tant que le fichier n'est pas fini ou la ligne n'est pas vide
        while not End_Of_File (File) loop
            if End_Of_Line (File) then
                Skip_Line (File);
            end if;

            Get (File, I);
            Get (File, J);

            Set (Adj, I + 1, J + 1, Defaut_Poid);
            Set (Sortants, I + 1, 1, Defaut_Poid);

        end loop;
    exception
        when End_Error =>
            null;
    end Lire_Graphe;

    procedure Ponderer_Graphe (Adj : in out T_Matrice; Sortants : in T_Matrice)
    is
        Item : T_Valeur;
    begin
        for I in 1 .. Adj.Lignes loop
            for J in 1 .. Adj.Colonnes loop
                Item := Get (Adj, I, J);
                if Item /= Zero then
                    Set (Adj, I, J, Item / Get (Sortants, I, 1));
                end if;
            end loop;
        end loop;
    end Ponderer_Graphe;
end Graphe;
