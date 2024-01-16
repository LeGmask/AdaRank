\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{NewTeXRaffinage}[A modern LaTeX template]

\LoadClass[11pt]{article}

%%%%%%%%%%%%%%%%%%
%%%  Packages  %%%
%%%%%%%%%%%%%%%%%%
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
\RequirePackage[french]{babel}
\RequirePackage{cancel}
\RequirePackage{lscape}
\RequirePackage{booktabs}
\RequirePackage{lmodern}
\RequirePackage{ragged2e}
\RequirePackage{multicol}
\RequirePackage{enumerate}
\RequirePackage{environ}
\RequirePackage{ifthen}

\newcommand{\tabitem}{~~\llap{\textbullet}~~}
\RequirePackage{tabularx}
\RequirePackage{longtable}
\RequirePackage{tabu}
\RequirePackage{ulem}
\newcolumntype{Y}{>{\centering\arraybackslash}X}


%%%%%%%%%%%%%%%
%%%  Fonts  %%%
%%%%%%%%%%%%%%%
\RequirePackage{amsmath,amsfonts,amsthm,amssymb}
\RequirePackage{mathrsfs}
\RequirePackage{wasysym}
\RequirePackage{fontspec}
\RequirePackage[most]{tcolorbox}
\RequirePackage{xcolor, color}
\tcbuselibrary{breakable}

%\newfontfamily{\serialfont}{Jaapokki}[
    %Extension=.otf,
%    UprightFont=*-Regular
%]

\setmonofont{Fira Code Light}[Contextuals=Alternate]

\newfontfamily{\rubikregfont}{Rubik}[
    Path=./RubikFontFiles/,
    Scale=0.9,
    Extension = .ttf,
    UprightFont=*-Regular,
    BoldFont=*-SemiBold,
    ItalicFont=*-Italic,
    BoldItalicFont=*-BoldItalic
    ]

\setmainfont{Rubik}[
    Path=./RubikFontFiles/,
    Scale=0.9,
    Extension = .ttf,
    UprightFont=*-Light,
    BoldFont=*-Bold,
    ItalicFont=*-LightItalic,
    BoldItalicFont=*-BoldItalic
    ]
    
\setromanfont{Rubik}[
    Path=./RubikFontFiles/,
    Scale=0.9,
    Extension = .ttf,
    UprightFont=*-Light,
    BoldFont=*-Bold,
    ItalicFont=*-Italic,
    BoldItalicFont=*-BoldItalic
    ]

%%%%%%%%%%%%%%%%
%%%  Tokens  %%%
%%%%%%%%%%%%%%%%
\newtoks\classe
\newtoks\course
\newtoks\theme
\newtoks\maincolor
\course{XXXXH-0000}
\theme{classic}
\maincolor{color_purple}

%%%%%%%%%%%%%%%%%%%
%%%  paragraph  %%%
%%%%%%%%%%%%%%%%%%%
\setlength{\parskip}{1em}

%%%%%%%%%%%%%%%%
%%%  Colors  %%%
%%%%%%%%%%%%%%%%
%\newtoks\defaultTheme
%\defaultTheme{classic}
%\newcommand{\themeCheck}{\theme}
%\newcommand{\themeCheckDef}{\defaultTheme}
%\ifthenelse{\equal{\themeCheck}{\themeCheckDef}}
%  {% True case
%    \definecolor{color_red}{HTML}{FF4340}
%    \definecolor{color_orange}{HTML}{FF9D75}
%    \definecolor{color_yellow}{HTML}{FFBD78}
%    \definecolor{color_green}{HTML}{7DFF4D}
%    \definecolor{color_darkgreen}{HTML}{7DB370}
%    \definecolor{color_teal}{HTML}{5DA38F}
%    \definecolor{color_purple}{HTML}{FF33E8}
%    \definecolor{color_lightblue}{HTML}{26A4FF}
%    \definecolor{color_darkblue}{HTML}{2476B3}
%    \definecolor{color_purple}{HTML}{BE6BAD}
%    \definecolor{color_lightgray}{HTML}{f2f2f2}
%  }
%  {% false case
%    \definecolor{color_red}{HTML}{FF6F5F}
%    \definecolor{color_orange}{HTML}{FF9D75}
%    \definecolor{color_yellow}{HTML}{FFBD78}
%    \definecolor{color_green}{HTML}{B6C783}
%    \definecolor{color_darkgreen}{HTML}{7DB370}
%    \definecolor{color_teal}{HTML}{5DA38F}
%    \definecolor{color_lightblue}{HTML}{3399CC}
%    \definecolor{color_darkblue}{HTML}{2D77E3}
%    \definecolor{color_purple}{HTML}{BE6BAD}
%    \definecolor{color_lightgray}{HTML}{f2f2f2}
%  }

\definecolor{color_red}{HTML}{FF6F5F}
\definecolor{color_orange}{HTML}{FF9D75}
\definecolor{color_yellow}{HTML}{FFBD78}
\definecolor{color_green}{HTML}{B6C783}
\definecolor{color_darkgreen}{HTML}{7DB370}
\definecolor{color_teal}{HTML}{5DA38F}
\definecolor{color_lightblue}{HTML}{3399CC}
\definecolor{color_darkblue}{HTML}{2D77E3}
\definecolor{color_purple}{HTML}{BE6BAD}
\definecolor{color_lightgray}{HTML}{f2f2f2}

%%%%%%%%%%%%%%%%%
%%%  Margins  %%%
%%%%%%%%%%%%%%%%%
\RequirePackage[a4paper, left=1cm, right=1cm, top=1cm, bottom=1cm]{geometry}

%%%%%%%%%%%%%%%%%%
%%%  Chapters  %%%
%%%%%%%%%%%%%%%%%%
\RequirePackage{titlesec}

%%%%%%%%%%%%%%%%%%
%%%  Drawings  %%%
%%%%%%%%%%%%%%%%%%
\RequirePackage{tikz}
\RequirePackage[american resistor, european voltage, european current, american inductor]{circuitikz}

%%%%%%%%%%%%%%%
%%%  Table  %%%
%%%%%%%%%%%%%%%
\renewcommand{\tabularxcolumn}[1]{m{#1}}
\renewcommand{\arraystretch}{1.5}

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Table of content  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{hyperref}
\hypersetup{
    colorlinks,
    citecolor=black,
    filecolor=black,
    linkcolor=black,
    urlcolor=black
}

%%%%%%%%%%%%%%%%%%
%%%  Chemical  %%%
%%%%%%%%%%%%%%%%%%
\RequirePackage[version=4]{mhchem}
\RequirePackage{chemfig}

%%%%%%%%%%%%%%%
%%%  Boxes  %%%
%%%%%%%%%%%%%%%
\newtcolorbox{definition}{
    breakable,
    enhanced,
    arc=6pt,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_red,
      colback=color_red,
      colupper={black},
    },
    fonttitle=\fontsize{11}{20}\rubikregfont\bfseries,
    title=\color{white}Définition
}

\newtcolorbox{propertie}{
    breakable,
    enhanced,
    arc=6pt,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_purple,
      colback=color_purple,
      colupper={black},
    },
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{white}Propriété
}

\newtcolorbox{properties}{
    breakable,
    enhanced,
    arc=6pt,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_purple,
      colback=color_purple,
      colupper={black},
    },
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{white}Propriétés
}

\newtcolorbox{theorem}{
    breakable,
    enhanced,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_darkblue,
      colback=color_darkblue,
      colupper={black},
    },
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{white}Théorème
}

\newtcolorbox{lemme}{
    breakable,
    enhanced,
    arc=6pt,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_lightblue,
      colback=color_lightblue,
      colupper={black},
    },
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{white}Lemme
}

\newtcolorbox{corollaire}{
    breakable,
    enhanced,
    arc=6pt,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_lightblue,
      colback=color_lightblue,
      colupper={black},
    },
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{white}Corollaire
}

\newtcolorbox{demonstration}{
    breakable,
    enhanced,
    colback=white,
    enhanced,
    frame hidden,
    borderline west = {2pt}{0pt}{color_purple},
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{black}Démonstration
}

\newtcolorbox{demo_corollaire}{
    breakable,
    enhanced,
    colback=white,
    enhanced,
    frame hidden,
    borderline west = {2pt}{0pt}{color_lightblue},
    fonttitle=\fontsize{10}{20}\rubikregfont,
    title=\color{black}Démonstration
}

\newtcolorbox{demo_theorem}{
    breakable,
    enhanced,
    colback=white,
    enhanced,
    frame hidden,
    borderline west = {2pt}{0pt}{color_darkblue},
    fonttitle=\fontsize{10}{20}\rubikregfont,
    title=\color{black}Démonstration
}

\newtcolorbox{sample}{
    breakable,
    enhanced,
    colback=white,
    enhanced,
    frame hidden,
    fonttitle=\fontsize{11}{20},
    title=\color{black}\underline{Exemple :}
}
\newtcolorbox{samples}{
    breakable,
    enhanced,
    colback=white,
    enhanced,
    frame hidden,
    fonttitle=\fontsize{11}{20},
    title=\color{black}\underline{Exemples :}
}

\newtcolorbox{vocabulaire}{
    breakable,
    enhanced,
    arc=6pt,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_green,
      colback=color_green,
      colupper={black},
    },
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{white}Vocabulaire,
}

\newtcolorbox{custombox}[2][]{
    %breakable,
    enhanced,
    colback=white,
    enhanced,
    frame hidden,
    fonttitle=\fontsize{11}{20},
    title=\color{black}\underline{#2},
    #1 %custom injected style
}

\newtcolorbox{remarque}{
    breakable,
    enhanced,
    arc=6pt,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_teal,
      colback=color_teal,
      colupper={black},
    },
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{white}Remarque,
}

\newtcolorbox{newtex_code_area}[1]{
    breakable,
    enhanced,
    arc=6pt,
    colback=color_lightgray,
    colframe=color_lightgray,
    boxrule=0mm,
    top=6mm,
    attach boxed title to top left={xshift=5mm,yshift=-4.5mm},
    boxed title style={enhanced,
      colframe=color_yellow,
      colback=color_yellow,
      colupper={black},
    },
    fonttitle=\fontsize{10}{20}\rubikregfont\bfseries,
    title=\color{white}#1,
}

%%%%%%%%%%%%%%%%%%%%
%%%  Code block  %%%
%%%%%%%%%%%%%%%%%%%%
\RequirePackage{minted}
\usemintedstyle{xcode}

\newenvironment{code}[2][]
 {\VerbatimEnvironment
  \begin{newtex_code_area}{#2}
  \begin{minted}[
    fontsize=\scriptsize,
    #1]{#2}}
 {\end{minted}\end{newtex_code_area}}

