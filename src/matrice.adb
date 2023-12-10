--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Matrice is
  procedure Init (Mat : out T_Matrice; Val : in T_Valeur := Neutre) is
  begin
    Mat := (others => (others => Val));
  end Init;

  --function Dim(Mat: in T_Matrice) return T_Dim is
  -- Mat_Dim: T_Dim;
  -- begin
  -- return (1 => Mat'Length(1), 2 => Mat'Length(2));
  -- end Dim;

  function "+" (A, B : in T_Matrice) return T_Matrice is
    Mat : T_Matrice (A'Range (1), A'Range (2));
  begin
    -- Init(Mat, 0);
    for I in Mat'Range (1) loop
      for J in Mat'Range (2) loop
        Mat (I, J) := A (I, J) + B (I, J);
      end loop;
    end loop;

    return Mat;
  end "+";

  function "*" (A, B : in T_Matrice) return T_Matrice is
    Mat : T_Matrice (A'Range (1), B'Range (2));
  begin
    for I in Mat'Range (1) loop
      for J in Mat'Range (2) loop
        Mat (I, J) := A (I, 1) * B (1, J);
        for K in 2 .. A'Length (2) loop
          Mat (I, J) := Mat (I, J) + A (I, K) * B (K, J);
        end loop;
      end loop;
    end loop;

    return Mat;
  end "*";

  function "*" (A : in T_Matrice; B : in T_Valeur) return T_Matrice is
    Mat : T_Matrice (A'Range (1), A'Range (2));
  begin
    for I in Mat'Range (1) loop
      for J in Mat'Range (2) loop
        Mat (I, J) := A (I, J) * B;
      end loop;
    end loop;

    return Mat;
  end "*";

  function "*" (A : in T_Valeur; B : in T_Matrice) return T_Matrice is
    Mat : T_Matrice (B'Range (1), B'Range (2));
  begin
    for I in Mat'Range (1) loop
      for J in Mat'Range (2) loop
        Mat (I, J) := A * B (I, J);
      end loop;
    end loop;

    return Mat;
  end "*";

  function Transpose (A : in T_Matrice) return T_Matrice is
    Mat : T_Matrice (A'Range (2), A'Range (1));
  begin
    for I in Mat'Range (1) loop
      for J in Mat'Range (2) loop
        Mat (I, J) := A (J, I);
      end loop;
    end loop;

    return Mat;
  end Transpose;

  procedure Afficher (Mat : in T_Matrice) is
  --  Header: constant String := "┌" & 3 * Mat'Length(2) * " " & "┐";
  --  Footer: constant String := "└" & 3 * Mat'Length(2) * " " & "┘";
  begin
    --  Put_Line(Header);
    for I in Mat'Range (1) loop
      Put ("│");
      for J in Mat'Range (2) loop
        Afficher_Valeur (Mat (I, J));
        Put (" ");
      end loop;
      Put_Line ("│");
    end loop;
    --  Put_Line(Footer);
  end Afficher;

end Matrice;
