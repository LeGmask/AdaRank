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

            Adj (I + 1, J + 1)  := Adj (I + 1, J + 1) + Defaut_Poid;
            Sortants (I + 1, 1) := Sortants (I + 1, 1) + Defaut_Poid;

        end loop;
    exception
        when End_Error =>
            null;
    end Lire_Graphe;

    procedure Ponderer_Graphe (Adj : in out T_Matrice; Sortants : in T_Matrice)
    is
    begin
        for I in Adj'Range (1) loop
            for J in Adj'Range (2) loop
                if Adj (I, J) /= Neutre then
                    Adj (I, J) := Adj (I, J) / Sortants (I, 1);
                end if;
            end loop;
        end loop;
    end Ponderer_Graphe;
end Graphe;
