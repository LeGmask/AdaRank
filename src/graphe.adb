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
            Set (Sortants, I + 1, 1, Get (Sortants, I + 1, 1) + Defaut_Poid);

        end loop;
    exception
        when End_Error =>
            null;
    end Lire_Graphe;

    procedure Ponderer_Graphe (Adj : in out T_Matrice; Sortants : in T_Matrice)
    is
        Dividende : T_Valeur;
        Diviseur : T_Valeur;
    begin
        for I in 1 .. Adj.Lignes loop
            Diviseur := Get (Sortants, I, 1);
            for J in 1 .. Adj.Colonnes loop
                Dividende := Get (Adj, I, J);
                if Dividende /= Zero then
                    Set (Adj, I, J, Dividende / Diviseur);
                end if;
            end loop;
        end loop;
    end Ponderer_Graphe;
end Graphe;
