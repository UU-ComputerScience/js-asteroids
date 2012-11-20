\appendix
\chapter{Appendix}

\section{XLib Hello World}
\label{app:xlib_helloworld}
\verbatiminput{resources/xlib_helloworld.cc}

\section{LightOO Macros}
\label{appendix:macros}

We define CPP macros for deriving the boilerplate for top-level classes and subclasses. To ease their definition we create a helper type class that allows us to capture the set of tail manipulation functions:

\begin{code}
class ModTail c where
   getTail :: c t -> Record t
   setTail :: c t -> Record t' -> c t'

   modifyTail :: (Record t -> Record t') -> c t -> c t'
   modifyTail = mkMod setTail getTail

mkMod set get f o = set o (f (get o))
\end{code}

For example, the instance for \emph{Shape} looks like this:

\begin{code}
instance ModTail IShape where
  getTail      = _shapeTail
  setTail o v  = o { _shapeTail = v }
\end{code}

The |ModTail| type class does not yet provide us with access to the tail of a subclass, but we can use it to derive a family of functions for nested tail manipulation by expressing their tail manipulation functions in terms of their parent's.

Here is an example of a family of functions derived for |IShape ()| and |IShape (IRectangle ())|: 

\begin{code}
-- Shape
get_Shape_Tail :: IShape a -> Record a 
get_Shape_Tail = getTail 

set_Shape_Tail :: IShape a -> Record b -> Shape_ b 
set_Shape_Tail o v = setTail o v 

modify_Shape_Tail = mkMod set_Shape_Tail get_Shape_Tail

-- Rectangle
get_Rectangle_Tail :: IShape (IRectangle a) -> Record a 
get_Rectangle_Tail = getTail . unRecord . get_Shape_Tail 

set_Rectangle_Tail :: IShape (IRectangle a) -> Record b -> IShape (IRectangle b)
set_Rectangle_Tail o v = modify_Shape_Tail (\o -> record $ setTail (unRecord o) v) o 

modify_Rectangle_Tail = mkMod set_Rectangle_Tail get_Rectangle_Tail 
\end{code}

Because the nested types can at times become quite lengthy we mold the types into a type synonym structure. 
This makes it easier for the programmer to read type errors, provide type annotations when necessary, and for the macros to generate code.

\begin{code}
type Shape_ t = IShape t
type Shape = Shape_ ()

type Rectangle_ t = Shape_ (IRectangle t)
type Rectangle = Rectangle_ ()
\end{code}

The \verb DefineClass  macro can be used to derive the boilerplate for top-level classes, \verb DefineSubClass  for subclasses. Their definition is somewhat complicated by the fact that they are also suited for parameterized classes.

\begin{verbatim}
#define DefineClass(X,XC,XTAIL,AP,NP)
#define DefineSubClass(X,Y,XC,XTAIL,AP,YP,XP,NP,CONSTR)
\end{verbatim}

\begin{itemize}
  \item $X$, the name of the class
  \item $Y$, the name of the parent class
  \item $XC$, the name of the data type representing the class
  \item $XTAIL$, is the name of the function for manipulating the tail
  \item $AP$, all type parameters except the tail
  \item $YP \subseteq AP$, all type parameters that distribute to $Y$
  \item $XP \subseteq AP$, all type parameters that distribute to $X$
  \item $NP = ||AP|| + 1$, the number of type parameters
  \item $CONSTR$, a listing of |Typeable| constraints on the $AP$ type parameters used for the |Narrow| and |Widen| instances
\end{itemize}


\begin{verbatim}
#define DefineClass(X,XC,XTAIL,P,NP) \
type X ## _ P t = XC P t ; \
type X P = X ## _ P () ; \
\
deriving instance Typeable ## NP XC ; \
\
instance ModTail (XC P) where { \
   getTail = _ ## XTAIL ; \
   setTail o v = o { _ ## XTAIL = v } } ; \
\
get_ ## X ## _Tail :: X ## _ P t -> Record t ; \
get_ ## X ## _Tail = getTail ; \
set_ ## X ## _Tail :: X ## _ P t -> Record tt -> X ## _ P tt ; \
set_ ## X ## _Tail o v = setTail o v ; \
modify_ ## X ## _Tail = mkMod set_ ## X ## _Tail get_ ## X ## _Tail ;

#define DefineSubClass(X,Y,XC,XTAIL,AP,YP,XP,NP,CONSTR) \
type X ## _ AP t = Y ## _ YP (XC XP t) ; \
type X AP = X ## _ AP () ; \
\
instance (CONSTR) => Narrow (X AP) (Y YP) where { \
   narrow = modify_ ## Y ## _Tail hideImpl } ; \
\
instance (CONSTR) => Widen (Y YP) (X AP) where { \
   widen o = genericWiden o get_ ## Y ## _Tail set_ ## Y ## _Tail } ; \
\
deriving instance Typeable ## NP XC ; \
\
instance ModTail (XC XP) where { \
   getTail = _ ## XTAIL ; \
   setTail o v = o { _ ## XTAIL = v } } ; \
\
get_ ## X ## _Tail :: X ## _ AP t -> Record t ; \
get_ ## X ## _Tail = getTail . headRecord . get_ ## Y ## _Tail ; \
\
set_ ## X ## _Tail :: X ## _ AP t -> Record tt -> X ## _ AP tt ; \
set_ ## X ## _Tail o v = 
  modify_ ## Y ## _Tail (\o -> consRecord $ setTail (headRecord o) v) o ; \
\
modify_ ## X ## _Tail = mkMod set_ ## X ## _Tail get_ ## X ## _Tail ;
\end{verbatim}