package body Export is

    procedure Export_Resultats
       (Pi   : in T_Matrice; Ordre : in Matrice_Ordre.T_Matrice;
        N, K : in Integer; Alpha : in Float; Prefix : in String)
    is
        Fichier_Pi    : File_Type;
        Fichier_Ordre : File_Type;

    begin
        Create (Fichier_Pi, Out_File, Prefix & ".prw");
        Put_Line
           (Fichier_Pi,
            Integer'Image (N) & " " & Float'Image (Alpha) & " " &
            Integer'Image (K));

        for J in 1 .. N loop
            Put_Pi (Fichier_Pi, Get(Pi, 1, J));
            New_Line (Fichier_Pi);
        end loop;
        Close (Fichier_Pi);

        Create (Fichier_Ordre, Out_File, Prefix & ".pr");
        for J in 1 .. N loop
            Put_Ordre (Fichier_Ordre, Matrice_Ordre.Get(Ordre, 1, J));
            New_Line (Fichier_Ordre);
        end loop;

        Close (Fichier_Ordre);
    end Export_Resultats;

end Export;
