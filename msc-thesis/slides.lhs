\documentclass[svgnames]{beamer}

%include polycode.fmt
%include lhs2TeX.fmt
%include haskell.fmt
%include forall.fmt
%include exists.fmt
%include beamer.fmt
%include beamerboxed.fmt

%format <|>     = "{\color{blue}\texttt{ <|> }}"
%format <+>     = "{\color{blue}\texttt{ <+> }}"
%format <?>     = "{\color{blue}\texttt{ <?> }}"
%format <*>     = "{\color{blue}\texttt{ <*> }}"
%format eagerly = "{\color{blue}eagerly}"
%format cut     = "{\color{blue}cut}"
%format stop    = "{\color{blue}stop}"
%format return  = "{\color{blue}return}"
%format liftIO  = "{\color{blue}liftIO}"
%format >>      = "{\color{blue}\texttt{ >> }}"
%format >>=     = "{\color{blue}\texttt{ >>= }}"
%format forall = "\forall"
%format -    = "$\texttt{-}$"
%format foreign                    = "\Keyword{foreign}"
%format js                         = "\Keyword{js}"
%format top              = "\top "
%format :|:              = " + "
%format :<:              = " \prec "
%format :>:                   = " \succ "
%format iso              = " \cong "
%format DefineClass        = "DefineClass_{macro}"
%format DefineSubClass        = "DefineSubClass_{macro}"
%format alpha                 = "\alpha"
%format beta                  = "\beta"
%format Comp                  = "\circ"
%format oplus                 = "\oplus"
%format equiv                 = "\equiv"
%format cp1                 = "cp_1"
%format cp2                 = "cp_2"
%format cv1                 = "cv_1"
%format clazz                 = "\Keyword{clazz}"
%format unRecord              = "\Keyword{unRecord}"
%format record                 = "\Keyword{record}"
%format extends               = "\Keyword{extends}"
%format _shapeTail  = "{\color{blue}\_shapeTail}"
%format _rectangleTail  = "{\color{blue}\_rectangleTail}"
%format _circleTail  = "{\color{blue}\_circleTail}"
%format noOverride               = "\Keyword{noOverride}"
%format new                   = "\Keyword{new}"
%format emptyRecord                   = "\Keyword{emptyRecord}"
%format #                   = "\Keyword{\#}"
%format self                = "{\color{purple}self}"
%format \_                  = "\lambda\underline{\hspace{0.3cm}}"
%format _                   = "\underline{\hspace{0.3cm}}"
%format tail                = "{\color{purple}tail}"
%format IShape              = "{\color{orange}IShape}"
%format IRectangle          = "{\color{orange}IRectangle}"
%format IObject              = "{\color{orange}IObject}"
%format ICircle              = "{\color{orange}ICircle}"
%format ext                 = "{\begin{tikzpicture}[scale=.5]\node[rectangle,draw] {$\rhd$};\end{tikzpicture}}"


\usetheme{Copenhagen}
\usecolortheme{dolphin}

\usepackage[english]{babel}
\usepackage[absolute,overlay]{textpos}
\usepackage{verbatim}
\usepackage{color}
\usepackage{tikz}
\usepackage{txfonts}
\usepackage{graphicx}
\usepackage{bussproofs}
\usepackage{semantic}
\usepackage{ulem}

\newcommand{\ppause}{\pause \vspace{ -2em}}
\newcommand{\ignore}[1]{}
\defbeamertemplate*{title page}{customized}[1][]
{
  \begin{center}
  \usebeamerfont{title}\inserttitle\par\vspace{5 pt}
  \usebeamerfont{subtitle}\usebeamercolor[gray]{subtitle}\small\insertsubtitle\par\vspace{10 pt}
  \usebeamerfont{author}\usebeamercolor[fg]{textnormal}\insertauthor\par
  \bigskip
  \usebeamerfont{institute}\insertinstitute\par
  \usebeamerfont{date}\insertdate\par
  \bigskip
  \usebeamercolor[fg]{titlegraphic}\inserttitlegraphic
\end{center}
}

\title{wxHaskell for the web}
\subtitle{Substituting C++ for Haskell and JavaScript}
\author{Ruben Alexander de Gooijer}
\institute[Dept. of CS @@ UU.nl]{{[}\textit{Supervisors}{]} Atze Dijkstra and Doaitse Swierstra \\ Utrecht University Department of Computing Science}
\date{October 4$^{\text{th}}$, 2012}

\begin{document}

{
\usebackgroundtemplate{\includegraphics[width=\paperwidth]{resources/bg.png}}
\begin{frame}
   \maketitle
\end{frame}
}

\setcounter{tocdepth}{1}
\begin{frame}
  \tableofcontents
\end{frame}

\section{Introduction}

\subsection{Problem}

\begin{frame}
\frametitle{Type-safe web applications}

Haskell on the server-side: Snap, Yesod, HappStack. 
\newline
{\pgfputat{\pgfxy(9.5,-2)}{\pgfbox[left,top]{\includegraphics[scale=.5]{resources/uhclogo.png}}}}

The \emph{U}trecht \emph{H}askell \emph{C}ompiler JavaScript back-end gives us Haskell on the client. 
\newline\newline
Haskell on the server and client equals profit!
\begin{itemize}
    \item Automatic data type consistency \newline
    {\color{gray} \small no more mapping problems}
    \item Code sharing \newline
    {\color{gray} \small no more duplication of validation, business rules, etc.}
    \item No JavaScript! \newline
    {\color{gray} \small JavaScript as assembly language}
\end{itemize}

% Context - the WHY

\end{frame}

\begin{frame}
\frametitle{O GUI toolkit, Where Art Thou?}

For client-side development we need a GUI toolkit. 
\newline\newline
It makes sense to reuse an existing approach:
\begin{itemize}
  \item Gtk2Hs (Linux) 
  \item wxHaskell (multi-platform)
\end{itemize}
\vspace{10 pt}
Both interface to {\bf foreign} GUI toolkits, but neither run in the web browser.

\end{frame}

\subsection{Research Question}

\begin{frame}
\frametitle{Research Question}

\begin{quote}
{\color{blue}
How can wxHaskell be made to run in the web browser?}
\end{quote}
\vspace{20pt}
We claim to have answered our research question through the implementation of a subset of wxHaskell that runs in the web browser.
\newline\newline
As a case study we made it possible to run a feature-light version of \emph{wxAsteroids} on the desktop as well as in the web browser.
\end{frame}

\subsection{Architecture}

\begin{frame}
\frametitle{Why wxHaskell?}

\begin{itemize}
  \item Structured into multiple layers \newline
  {\color{gray} \small Will make porting easier}
  \pgfputat{\pgfxy(4,2)}{\pgfbox[left,top]{\includegraphics[scale=.3]{resources/uulogo.png}}}
  \item Abstractions \newline
  {\color{gray} \small More declarative programming-style compared to Gtk2Hs}
  \item Already has multi-platform support \newline
  {\color{gray} \small The web as yet another platform}
\end{itemize}
\vspace{20pt}
History in Utrecht \newline
{\color{gray} \small Daan Leijen, the original author of wxHaskell is a former UU PhD}

\end{frame}

\begin{frame}
\frametitle{wxHaskell Architecture}
\begin{columns}
    \begin{column}{.2\textwidth}
      \includegraphics[scale=.4]{resources/wxhaskell-layers.png}
    \end{column}
    \begin{column}{.8\textwidth}
        \begin{itemize}
          \item wx \newline
          {\color{gray} \small abstractions for \emph{parts} of wxCore}
          \item wxCore \newline
          {\color{gray} \small a Haskell interface to wxWidgets using wxC}
          \item wxC \newline
          {\color{gray} \small automatically generated Foreign Function Interface (FFI) bindings to wxWidgets}
          \item wxWidgets \newline
          {\color{gray} \small an API for cross a platform GUI toolkit}
          \item Platform API \newline
          {\color{gray} \small native GUI libraries: GTK, Cocoa, Windows}
        \end{itemize}
    \end{column}
  \end{columns}

\end{frame}

\begin{frame}
\frametitle{wxHaskell for the web}

\begin{columns}
\begin{column}{.4\textwidth}
  \includegraphics[scale=.38]{resources/wxweb_question.png}
\end{column}
\begin{column}{.6\textwidth}

What do we want?
\begin{itemize}
  \item Easily transfer existing wxHaskell programs to the web
  \item A portable solution
  \item The benefits of Haskell \newline
  {\color{gray} \small type-safety, compiler optimizations...}
\end{itemize}
\vspace{10 pt}
We cannot reuse wxWidgets! 
\begin{itemize}
  \item Reimplement it in JavaScript?
  \item Or in Haskell?
\end{itemize}

\vspace{10 pt}
{\tiny Blue: Haskell, Orange: C++, Yellow: JavaScript}
\end{column}
\end{columns}

\end{frame}

\begin{frame}
\frametitle{Solution}

Implement the \emph{wxcore} API in Haskell:
\newline\newline
Plan of attack:
\begin{itemize}
  \item Create a library for OO programming in Haskell 
  \item Use it to implement the wxcore API (OO design)
  \item Interface to the web browser using the JavaScript FFI
  \begin{code}
  foreign import js "%1.alert(%2)"
    alert :: Window -> JSString -> IO ()
  \end{code}
  \item Compile everything with UHC to JavaScript.
\end{itemize}

\end{frame}

\section{Object-Oriented programming in Haskell}

\subsection{Introduction}

\begin{frame}
{\pgfputat{\pgfxy(0,0)}{\pgfbox[left,top]{\includegraphics[scale=.8,angle=25]{resources/oodiagram.png}}}}
\vspace{-30pt}
\huge Object-Oriented programming \newline in Haskell \newline
\small \color{gray} are you mental?
\end{frame}

\begin{frame}
\frametitle{Introduction}

The library is based on the \emph{"Mutable Objects, with tail-polymorphism"} approach described in the OOHaskell paper.
\newline\newline
We have extended their approach with:
\begin{itemize}
  \item A proper implementation of inheritance
  \item Generic functions for casting
  \item Parameterized classes
  \item Macros for deriving parts of the boilerplate
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Introduction}
% we are not going to show the impl of circle as it is similar to that of rectangle
\begin{columns}
  \begin{column}{.6\textwidth}
    \includegraphics[scale=.50]{resources/shapes.png}
  \end{column}
  \begin{column}{.4\textwidth}
    \emph{The Shapes Benchmark} \newline\newline
    A benchmark for testing a language's ability to express:
    \begin{itemize} 
      \item Data encapsulation
      \item Inheritance
      \item Subtype polymorphism
      \item Abstract methods
    \end{itemize}
  \end{column}
\end{columns}

\end{frame}

\begin{frame}
\frametitle{Introduction}
Object encoding: \newline
\begin{itemize}
  \item Objects as (plain) records of closures
  \begin{itemize}
    \item record selectors $\rightarrow$ method implementations
    \item closure $\rightarrow$ data encapsulation
  \end{itemize}
\end{itemize}
  \vspace{10 pt}
Combining records:
\vspace{10 pt}
  \begin{itemize}
    \item Type extension through tail-polymorphism
    \begin{itemize}
      \item poorman's approach to extensible records
      \item type parameter represents a record extension (the tail)
      \item special selector for manipulating the tail
    \end{itemize}
  \end{itemize}

\end{frame}

\subsection{Shapes}

\begin{frame}
\frametitle{Object Types}
\renewcommand{\hscodestyle}{\small}
\begin{columns}
  \hspace{-30pt}
  \begin{column}{.5\textwidth}
    \begin{code}
    data IShape alpha = IShape {
          _getX        :: IO Int
       ,  _getY        :: IO Int
       ,  _setX        :: Int -> IO ()
       ,  _setY        :: Int -> IO ()
       ,  _moveTo      :: Int -> Int -> IO ()
       ,  _draw        :: IO ()
       ,  _shapeTail  :: alpha
    }
    \end{code}
  \end{column}
  \begin{column}{.5\textwidth}
    \begin{code}
    data IRectangle alpha = IRectangle {
        _getWidth       :: IO Int
       ,_getHeight      :: IO Int
       ,_setWidth       :: Int -> IO ()
       ,_setHeight      :: Int -> IO ()
       ,_rectangleTail  :: alpha
    }
    \end{code}
    \begin{code}
    data ICircle alpha = ...
    \end{code}
  \end{column}
\end{columns}
\end{frame}

\begin{frame}
\frametitle{Record Combination}
\renewcommand{\hscodestyle}{\small}

Creating a rectangle out of |IShape| and |IRectangle|.

\begin{code}
IShape  {  _setX        = ...
        ,  _setY        = ...
        ,  _moveTo      = ...
        ,  _draw        = ...
        ,  _shapeTail  = ?
        } 
\end{code}

\end{frame}

\begin{frame}
\frametitle{Record Combination}
\renewcommand{\hscodestyle}{\small}

\begin{code}
IShape  {  _setX        = ...
        ,  _setY        = ...
        ,  _moveTo      = ...
        ,  _draw        = ...
        ,  _shapeTail   = IRectangle  {  _getWidth       = ...
                                      ,  _getHeight      = ...
                                      ,  _setWidth       = ...
                                      ,  _setHeight      = ...
                                      ,  _rectangleTail  = ()
                                      }
                
        } :: IShape (IRectangle ())
\end{code}

\end{frame}

\begin{frame}
\frametitle{Subtype Polymorphism}

Subtype polymorphism by quantifying over the tail:
\newline\newline
\vspace{10pt}
\begin{tikzpicture}[level distance=15mm,level/.style={sibling distance=40mm},]
\node {|IShape ()|}
  child {
    node {|IShape (IRectangle ())|}
  }
  child {
    node {|IShape (ICircle ())|}
  };
  \draw[->] (-5,.5) -- (-5,-2) node[midway,below,left] {|IShape alpha|};
\end{tikzpicture}

Foo takes \emph{at least} a Shape:
\begin{code}
foo :: IShape alpha -> ...
\end{code}

Restriction: polymorphic object types can only be used at the contravariant (consuming) position.

\end{frame}

\begin{frame}
\frametitle{Method Lookup}

Method implementation:
\begin{code}
_getWidth :: IRectangle alpha -> IO Int
\end{code}

Method lookup:
\begin{code}
getWidth :: IShape (IRectangle alpha) -> IO Int
getWidth = _getWidth . _shapeTail
\end{code}

\end{frame}

\begin{frame}
\frametitle{Implementation}
\renewcommand{\hscodestyle}{\small}
\begin{itemize}
  \item class implementation $\rightarrow$ function
  \item mutable variables $\rightarrow$ |IORef|
\end{itemize}

\begin{code}
shape newx newy concreteDraw = do
  x <- newIORef newx
  y <- newIORef newy
  return IShape {
       _getX        =  readIORef x 
    ,  _getY        =  readIORef y
    ,  _setX        =  writeIORef x
    ,  _setY        =  writeIORef y

    ,  _moveTo      =  \newx newy -> do
        ??
    ...
  }
\end{code}

\end{frame}

\begin{frame}
\frametitle{Self-reference}

\begin{itemize}
  \item invoke methods of the same object
\end{itemize}

\begin{code}
shape newx newy concreteDraw self = do
  ...
  return IShape {
    ...

    , _moveTo     = \newx newy -> do
        setX self newx
        setY self newy
    
    , _draw        = concreteDraw self

    ...
  }
\end{code}

\end{frame}


\begin{frame}
\frametitle{Record Extension}

|tail| is a computation that results in the extension of the record.

\begin{code}
shape newx newy concreteDraw tail self = do
  ...
  t <- tail
  return IShape {
    ...
    , _shapeTail = t
  }
\end{code}

\end{frame}

\begin{frame}
\frametitle{Let's see the type}
\renewcommand{\hscodestyle}{\small}

\begin{definition}
A \emph{class} is a \emph{function} that provides an implementation for an object.

\begin{code}
type Class tail self obj = IO tail -> self -> IO obj
\end{code}
\end{definition}

\vspace{20pt}
{\small In the case of |shape|, |self| and |obj| are specialized to \emph{at least} a Shape:}

\begin{code}
shape 2 3 (\_ -> print 42) :: Class tail (IShape alpha) (IShape tail)
\end{code}

\end{frame}

\subsection{Instantiation}

\begin{frame}
\frametitle{Instantiation}

We instantiate a class by:
\begin{itemize}
  \item closing record extension
  \item and providing a self-reference
\end{itemize}
\vspace{10pt}
Closing record extension with the empty record:

\begin{code}
emptyRecord :: IO ()
emptyRecord = return ()
\end{code}

Apply |shape| to |emptyRecord|:

\begin{code}
shape 2 3 drawImpl emptyRecord :: IShape alpha  -> IO (IShape ())
\end{code}

\end{frame}

\begin{frame}
\frametitle{Fixing the self-reference}

Before instantiation |self| is unbound:
\newline\newline
\includegraphics[scale=.30]{resources/self.png}
\newline
We connect |self| back to the class by taking its \emph{fixed point}:

\begin{code}
fixIO :: (o -> IO o) -> IO o
\end{code}

Apply fixIO to |shape|:

\begin{code}
fixIO $ shape 2 3 drawImpl emptyRecord :: IO (IShape ())
\end{code}

\end{frame}

\begin{frame}
\frametitle{The new combinator}

The |new| combinator takes a class and instantiates it.

\begin{code}
new c = fixIO $ c emptyRecord
\end{code}

E.g.

\begin{code}
do  let drawImpl _ = print 42
    s <- new $ shape 2 3 drawImpl
    draw s
\end{code}

\end{frame}

\subsection{Inheritance}

\begin{frame}
\frametitle{Inheritance}

{\bf What is inheritance?} \newline
A technique for sharing behavior between objects. 
\newline

{\bf How?} \newline
By subclassing, i.e. the incremental extension of classes.
\newline

{\bf Requirements:}
\begin{itemize}
  \item The self-reference should be \emph{late-bound} \newline
  {\color{gray} \small method invocations made by a superclass can be intercepted by a subclass.}
  \item The addition of methods and data to classes \emph{possibly} overriding existing methods
  \item A subclass can invoke methods on the superclass
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Cooking up an inheritance combinator}

William R. Cook, a denotational semantics of inheritance:
\begin{itemize}
  \item A combinator for inheritance using records in the untyped lambda calculus
\end{itemize}
\vspace{20pt}
\begin{code}
W ext G = \self -> W (G self) self oplus (G self)
\end{code}

\end{frame}

\begin{frame}
\frametitle{Cooking up an inheritance combinator}

William R. Cook, a denotational semantics of inheritance:
\begin{itemize}
  \item A combinator for inheritance using records in the untyped lambda calculus
\end{itemize}
\vspace{20pt}
\begin{code}
W ext G =
\end{code}

{\pgfputat{\pgfxy(2.5,1.75)}{\pgfbox[left,top]{\includegraphics[scale=.25]{resources/inhcomb.png}}}}

\end{frame}

\begin{frame}
\frametitle{Implementing Rectangle}

\begin{code}
rectangle x y width height =
   (rectImpl `extends` shape x y draw) noOverride set_Shape_Tail
   where
   rectImpl tail super self = do
    ...
    return IRectangle { 
      ... 
    }
\end{code}

\end{frame}

\subsection{Casting}

\begin{frame}
\frametitle{A small OOP example}

\begin{code}
myOOP = do
  s1 <- new $ rectangle 10 20 5 6
  s2 <- new $ circle 15 25 8

  let  scribble :: [?]
       scribble = ?

  mapM_ draw scribble   
\end{code}

\end{frame}

\begin{frame}
\frametitle{A small OOP example}
What do we put at the question mark?

\pause 

\begin{code}
[s1, s2] -- Type error
\end{code}

\pause 

Explicitely tag the values?

\begin{code}
scribble :: [Either (IShape (IRectangle ())) (IShape (ICircle ()))]
scribble = [Left s1, Right s2]
\end{code}

\end{frame}

\begin{frame}
\frametitle{A small OOP example}
Or existentially quantify over the tail?

\begin{code}
scribble :: [exists a. IShape a]
scribble = [s1, s2]
\end{code}

But... we cannot recover from the lost type information.

\end{frame}

\begin{frame}
\frametitle{A small OOP example}
Instead we implement two generic casting functions:

\begin{code}
upcast     :: alpha -> beta

downcast   :: beta -> Maybe alpha
\end{code}

Provided with a \emph{source} and \emph{target}, related by subtyping, they generate a proof that the 
two types can \emph{in principle} be converted to each other.
\end{frame}

\begin{frame}
\frametitle{Finally...}

\begin{code}
scribble :: [IShape ()]
scribble = [upcast s1, upcast s2]
\end{code}

\end{frame}

\begin{frame}
\frametitle{A glimpse at the internals}
\begin{itemize}
  \item |upcast| and |downcast| implemented using a type class
  \item Instances provide a syntax directed encoding of \emph{refl.} and \emph{trans.} of the subtyping relation by pattern matching on the type structure
  \item Changing the type of a record's tail to:
\begin{code}
type Record alpha = Either alpha Dynamic
\end{code}
  \item Interplay between |upcast| and |downcast| using dynamic typing to leave a trace to the original type.
\end{itemize}
\end{frame}


\begin{frame}
\frametitle{A glimpse at the internals}
\renewcommand{\hscodestyle}{\small}

\begin{columns}
\begin{column}{0.5\textwidth}
\begin{code}
class a :<: b where
   upcast :: a -> b
\end{code}
\end{column}
\begin{column}{0.5\textwidth}
\begin{code}
class a :>: b where
   downcast :: a -> Maybe b
\end{code}
\end{column}
\end{columns}

\begin{code}
-- Reflexivity
instance a () :<: a () where
   upcast = id

instance a () :>: a () where
   downcast = Just
\end{code}

\begin{code}
-- Transitivity
instance (a () :<: x, Narrow (a (b ())) (a ())) => a (b ()) :<: x 

instance (a (b ()) :>: a (b c), Widen (a ()) (a (b ()))) => a () :>: a (b c) 
\end{code}

\end{frame}
\section{Porting wxHaskell to the web browser}

\begin{frame}
\huge Porting wxHaskell \newline to the web browser\newline
\small \color{gray} a case study wxAsteroids
\end{frame}

\subsection{The Result}

\begin{frame}
\frametitle{After some hard work...}
\begin{figure}
\includegraphics[scale=.32]{resources/WxAsteroids.png}
\hspace{1 pt}
\includegraphics[scale=.32]{resources/ubuntu_wxasteroids.png}
\hspace{1 pt}
\includegraphics[scale=.32]{resources/browser_wxasteroids.png}
\end{figure}

\center Original \hspace{20 pt} $\longrightarrow$ \hspace{20 pt} Simplified \hspace{20 pt} $\longrightarrow$ \hspace{20 pt} In the browser.
\end{frame}

\subsection{Details}

\begin{frame}
\frametitle{Implementation details}

\begin{itemize}
  \item wxHaskell uses phantom types to model a type-safe interface to C++ objects.
  \item Our object encoding follows the exact same type structure, hence we simply replace the
        pointer structure with objects implemented in Haskell.
  \item At some places we needed to make the \emph{wxcore} interface less polymorph such that we could
        still use casting.
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Implementation details}

\begin{itemize}
  \item Cyclic type dependencies break organizational scheme: a module per interface and class.
  \item We choose a straight-forward mapping from wxWidgets abstractions to HTML5.
  \small
  \begin{itemize}
    \item wxWindow $\rightarrow$ HTML DIV
    \item Drawing Context $\rightarrow$ HTML Canvas
    \item Events $\rightarrow$ native events wrapped in wxWidgets event objects 
    \item Timer $\rightarrow$ setInterval
  \end{itemize}
\end{itemize}

\end{frame}

\section{Conclusion}

\begin{frame}
\frametitle{Conclusion}

wxHaskell can be made to run in the web browser.
\newline\newline
However,
\begin{itemize}
  \item Different forms of subtyping and the use of plain records makes the library not particularly easy to use. 
  \item The lack of recursive modules will make any attempt at OO programming in Haskell feel clumsy.
  \item It remains unclear if wxWidgets provides the right abstractions for the web platform.
\end{itemize}

\end{frame}

\begin{frame}

{\huge Questions?}
\newline\newline

OO library: \newline
\url{https://github.com/rubendg/lightoo}
\newline\newline
JS prelude: \newline
\url{https://github.com/rubendg/uhc-js}
\newline\newline
wxAsteroids: \newline
\url{https://github.com/rubendg/wxasteroids}
\end{frame}

\begin{frame}
\frametitle{Related work}

\begin{itemize}
  \item wxFlashkell: Building Flash based GUI's in Haskell
  \item GHCJS, Haste, YHC, Fay
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Future Work}

\begin{itemize}
  \item A translation from Featherweight Java to our OO library?
  \item Automated type checking based on FFI types
  \item Reuse |Data.Dynamic| for type checking? Building a JavaScript |TypeRep|?
  \item A more granular model for JavaScript types in Haskell, structural types?
\end{itemize}

\end{frame}


\begin{frame}
\frametitle{A combinator for inheritance}
\renewcommand{\hscodestyle}{\small}
\begin{code}
extends w g override oplus = clazz $ \tail self -> do
  -- instantiate super
  super    <- g emptyRecord self        
  -- provide the subclass with super and self
  sub      <- w tail super self         
  -- possibly override methods
  super'   <- override super self         
  -- combine the modified super and subclass records
  return $ super' oplus sub
\end{code}
\end{frame}

\end{document}
