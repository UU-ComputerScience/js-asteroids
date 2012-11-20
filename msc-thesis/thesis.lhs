\documentclass[10pt,twoside,a4paper,openright]{report}

\usepackage{fancyhdr}
\usepackage{xcolor}
\usepackage[utf8x]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{amsfonts}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsthm}
\usepackage{hyperref}
\usepackage{graphicx}
\usepackage{verbatim}
\usepackage[parfill]{parskip}
\usepackage{txfonts}
\usepackage{tikz}
\usepackage{xspace}
\usepackage{alltt}
\usepackage{multicol}
\usepackage{fancyvrb}
% ty rules
\usepackage{bussproofs}
\usepackage{semantic}
% things for the semantic package
\reservestyle{\command}{\textbf}
\command{let,in,:,case,of,if,then,else,letrec,nil,cons,false,true,[]}
\mathlig{-->}{\longrightarrow}
\newcommand{\tyrel}{\sqsubseteq}

\newcommand{\bigO}[1]{\mathcal{O}{\left(#1\right)}}

\newtheorem{definition}{Definition}

%include polycode.fmt
%include lhs2TeX.fmt
%include haskell.fmt
%include thesis.fmt
%include forall.fmt
%include exists.fmt

\newcommand{\js}[0]{JavaScript\xspace}
\newcommand{\jss}[0]{JavaScripts\xspace}
\newcommand{\oohaskell}[0]{{\rm OO}Haskell\xspace}
\renewcommand{\familydefault}{\sfdefault}

\renewenvironment{quote}
{\list{}{\rightmargin\leftmargin}%
\item\relax\small\begin{em}}
{\end{em}\endlist}

\DefineVerbatimEnvironment%
{OCaml}{Verbatim}
{xleftmargin=10mm,numbers=left,numbersep=2mm,
frame=single,label=OCaml}

\pagestyle{headings}

\pagestyle{fancy}
\renewcommand{\sectionmark}[1]{\markright{\thesection.\ #1}}
\renewcommand{\chaptermark}[1]{\markboth{\ #1}{}}

\bibliographystyle{plain}

\date{\today}

\begin{document}

\begin{titlepage}

\begin{center}
  \text{\large Master of Science Thesis}\\[2cm]
  \text{\Huge wxHaskell for the web}\\[0.5CM]
  \text{\large \emph{Substituting C++ with Haskell and \js}}\\[4.5CM]
  \text{\large by}\\[0.5cm]
  \text{\Large Ruben Alexander de Gooijer}\\[0.5cm]
  \large \today\\[0.5cm]
  % \text{INF/SCR-09-60}\\[0.5cm]
  \vfill
  \includegraphics[scale=.5]{resources/uulogo.png}\\[1cm]
\end{center}

\begin{minipage}{0.7\textwidth}
\begin{flushleft} \large
Center for Software Technology\\
Dept. of Information and Computing Sciences\\
Utrecht University\\
Utrecht, the Netherlands
\end{flushleft}
\end{minipage}
\begin{minipage}{0.4\textwidth}
\begin{flushright} \large
\emph{Daily Supervisor:} \\
dr. A. Dijkstra \\
\emph{Second Supervisor:} \\
prof. dr. S.D. Swierstra
\end{flushright}
\end{minipage}

\end{titlepage}

\begin{abstract}
Traditionally applications were built to run on top of a desktop platform, but this is changing rapidly and the web is now becoming the default deployment platform. Especially, with the new HTML5 standard the web becomes an even more attractive platform for many hybrid client/server applications. In Haskell one of the goto libraries for building graphical user interfaces is wxHaskell. We are motivated by the idea of using the high level abstractions of wxHaskell to develop type-safe client-side web applications in Haskell. With the recent advent of a \js back-end for UHC this has become an attainable goal. As a proof of concept we have ported a feature-light version of the wxAsteroids game from the original wxHaskell paper to the browser leaving the source code almost untouched. We have developed several tools that have helped us realizing the implementation. First, we improved the existing \js FFI, its surrounding infrastructure, and created interfaces to the necessary platform interfaces. Second, we have developed a library for Object-Oriented programming in Haskell, inspired by OOHaskell, that contrary to OOHaskell does not dependent on functional dependencies. This library has enabled us to maintain the wxHaskell interfaces while substituting the wxWidgets C++ implementation for one written in Haskell implemented in terms of HTML5.

\end{abstract}

\clearpage

\tableofcontents

%include chapters/introduction.lhs
%include chapters/background.lhs
%include chapters/architecture.lhs
%include chapters/jsffi.lhs
%include chapters/oo.lhs
%include chapters/implementation.lhs
%include chapters/conclusion.lhs

\chapter*{Acknowledgements}
\thispagestyle{empty}

At the end of my bachelor's study at the Applied Computer Science University Utrecht I had doubts about whether to pursue a master degree or not. Due to the discussions with my American colleges at the time, and my working experience among former university graduates who had greatly impressed me with their technical skills, I decided it was worth a try. 

Getting through the courses has been a bumpy ride to say the least. I had to overcome quite a few gaps in my background knowledge. At some point I nearly lost confidence, but thanks to the wonderful Andres L\"{o}h I regained trust in that everything would work out fine. I experienced Andres as an enthusiastic, knowledgeable, and excellent teacher with a great knack for understanding what you do not understand, but are not yet able to express. Writing the thesis, doing research, has been a great learning experience. Especially, being more practically minded, it took some time to get comfortable with the mindset necessary for literally spending weeks on something without having a clue whatsoever if it will result in anything to show for.

First of all I would like to thank my supervisors Atze Dijkstra and Doaitse Swierstra for helping me with this project, and Atze for going out of line by improving UHC even when time would not allow it. Without UHC this project would not have been possible! I want to thank Sean Leather for proofreading my thesis and providing useful suggestions and criticism. Many thanks also to Jurrien Stutterheim and Alessandro Vermeulen for the discussions on the \js back-end and their continuous effort to stay involved. Of course my thesis could not have been finished without a healthy dose of saccharine at the Pie-Thursdays, many thanks roomies Paul van der Walt, Ruud Koot, and Sjoerd Timmer. I sure did not intend to leave a mark on your theses the day you ate my rum-drenched tipsy cake! Finally, I would like to thank my family and friends for supporting me and listening to my endless ramblings about Haskell and otherwise unrelated topics.

\clearpage

\vspace*{\fill}
{\center\emph{"Haskell is the world’s finest imperative programming language"}, Simon P. Jones}

%include chapters/appendix.lhs

\bibliography{thesis}

\end{document}