\documentclass[xcolor=dvipsnames,sans,mathserif]{beamer}

\usepackage[english]{babel}
\usepackage{mathpazo}

%include polycode.fmt
%include code.fmt

\setbeamersize{text margin left=.5cm}
\setbeamersize{text margin right=.5cm}
\setbeamertemplate{navigation symbols}{}
\setlength\parindent{0.0in}
\setlength\parskip{0.25in} 

% -----------------------------------------------------------------------------

\definecolor{stress}{rgb}{0.60,0.60,1.00} 
\definecolor{c1}{rgb}{1.00,0.70,0.30} 

\setbeamercolor{title}{fg=c1}
\setbeamercolor{frametitle}{fg=c1}
\setbeamercolor{normal text}{fg=white}
\setbeamercolor{background canvas}{bg=black}

\usefonttheme[stillsansseriftext]{serif} 
\setbeamerfont{frametitle}{family=\rmfamily,shape=\itshape} 

\newcommand{\stress}[1]{\textcolor{stress}{#1}}

% -----------------------------------------------------------------------------

\begin{document}

\title{AwesomePrelude}
\subtitle{``Liberating Haskell from data types!''}
\author{Tom Lokhorst, Sebastiaan Visser} 
\date{\today} 

\frame{\titlepage} 

% -----------------------------------------------------------------------------

\begin{frame}

\center{\Huge{$4 + 3 \times 2$}}

\end{frame}

%include ArithExpr.lhs

%include Expr.lhs

%include BoolLike.lhs

\end{document}
