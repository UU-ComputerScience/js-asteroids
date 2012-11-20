\chapter[A lightweight OO DSL]{A lightweight approach to Object-Oriented programming in Haskell}
\label{chap:lightoo}

We are motived to explore the possibilities of Object-Oriented (OO) programming in Haskell by our desire to port  wxWidget's design to Haskell. While Haskell was not originally envisioned as a language for OO programming there have been several attempts at forging it into a OO language either by: extending the language with subtype polymorphism \cite{Nordlander01polymorphicsubtyping}, or embedding a DSL (OOHaskell) with the help of some common language extensions \cite{OOHaskell}. The latter approach has shown that Haskell's type-class-bounded and parametric polymorphism together with multi-parameter type classes and functional dependencies is expressive enough to model even the more advanced features of modern OO languages. 

Because we do not intend to extend Haskell our interest goes out to \oohaskell. The model used by \oohaskell to encode objects and their types is based on polymorphic, extensible records of closures, and favors encapsulation by procedural abstraction over existential types \cite{Pierce_object-orientedprogramming}. Haskell records are not polymorphic and extensible, hence \oohaskell makes heavy use of the HList library \cite{hlist} which models polymorphic extensible records through advanced type-level programming using functional dependencies. In their paper the authors also discuss several more primitive OO encodings. With one standing out in particular as bearing most resemblance with \oohaskell. It is described in section 3.4 \emph{"Mutable objects, with tail polymorphism"} and attempts to reify extensible records on top of regular records by leaving the so called tail of the record polymorph. After discussing the alternative encodings the authors venture into the more involved aspects of OO programming using the superior HList encoding. Several subjects such as code reuse, casting, self-returning methods, and more advanced forms of subtyping are left undiscussed for the more primitive encodings discharging them as: involved, requiring lots of boilerplate, or even infeasible. 

It would make sense to reuse \oohaskell were it not that UHC does not yet support functional dependencies. Driven by the practical necessity of a working OO approach not dependent on functional dependencies we attempt to stretch the possibilities of the \emph{"Mutable objects, with tail polymorphism"} approach and try to make its limitations manifest by submitting it to the different scenarios \oohaskell is submitted to. The outline and examples in this chapter will therefore, to a large extent, be shamelessly based on \oohaskell.  

Our contribution consists of an extensive exploration of the \emph{"Mutable objects, with tail polymorphism"} approach where we augment the original approach with a generic up and downcast operation and a combinator for expressing inheritance. Furthermore, we generalize the approach to a restricted form of parameterized classes. All results are bundled inside a ready to use library which comes with useful combinators and macros for deriving some of the boilerplate.

\section{Introduction}

\subsection{What is Object-Oriented programming?}

The fundamental concepts of OO programming originated in Simula 67 \cite{Holmevik:1994:CSH:612489.612573}. Later Smalltalk \cite{Goldberg:1983:SLI:273} extended and refined the concepts of Simula 67 by treating everything as an object (even a class) and uniformly interpreted all operations as passing messages to objects. Since Simula 67 and Smalltalk, OO languages have evolved and many varieties exist today which would make it pretentious to suggest we can provide a fitting answer to the section's title. However, there exists a common conception among researchers about what features are typically found in OO languages. According to Benjamin C. Pierce \cite{tpl} the fundamental feature set consists of:

\begin{enumerate}
  \item {\bf Multiple representations}. Objects with the same interface may use entirely different representations, i.e. an object interface is an abstract representation of the many possible instantiations. Method invocation works irrespective of the object representation. 

  \item {\bf Encapsulation}. The internal representation of an object is hidden such that only its methods may access it. 

  \item {\bf Subtyping}. The type of an object can be described by its interface, nominal name, or both. Often we want to write functionality which depends only on a part of an object's type. It would be too restrictive if we limit the functionality to work on \emph{exactly} one object type. Subtyping loosens this restriction allowing  functionality to work for many types as long as the expected type is related to the given type through the subtyping relation.

  \item {\bf Inheritance}. It is common for objects to share behavior with other objects. Inheritance is a mechanism which allows a particular form of behavior sharing; it accomplishes this by allowing the incremental extension of classes with the possibility to override pre-existing behavior. A class acts as a template for object instantiation. By extending a class we obtain a subclass which is just a regular class. 

  \item {\bf Open recursion}. Typically, an OO language allows the body of an object method to refer to other methods of the same object using a special identifier usually called \emph{this} or \emph{self}. In combination with inheritance it is essential that \emph{self} is \emph{late-bound} allowing it to refer to methods defined at a later point.
\end{enumerate}

\subsection{Outline}

In the section \ref{sec:shapes} we provide a high-level overview of the library. In section \ref{sec:tp-rec} we incrementally develop the type-independent part of the library. In section \ref{sec:ty-per} we look at the library from a type perspective and develop generic casting operations, discuss self-returning methods, and generalize the approach to a restricted form of parameterized classes. In section \ref{sec:discussion} we conclude with a discussion about the usability and efficiency of the library and provide some directions for future work. 

\section{The `shapes` example}
\label{sec:shapes}

The `shapes` example \footnote{See \url{http://onestepback.org/articles/poly/} for a multi-lingual collection of implementations in both OO as well as non-OO languages} combines the typical aspects found in OO languages into a single crisp benchmark.

\begin{figure}[h]
\center
\includegraphics[scale=.6]{resources/shapes.png}
\caption{An UML diagram for the shapes example. The boxes are subdivided into three compartments. The top-level compartment contains the class name, beneath it is a list of member variables prefixed with +/- respectively public or private, and at the bottom a list of methods. The arrows indicate an inheritance relationship, and bold faced text denotes an abstract method.}
\label{fig:shapesuml}
\end{figure}

Figure \ref{fig:shapesuml} shows the abstract class \emph{Shape} with two concrete subclasses \emph{Rectangle} and \emph{Circle}. A \emph{Shape} maintains a position and provides methods to directly \emph{moveTo} a new position, move relative to current position \emph{rMoveTo}, or \emph{draw} the \emph{Shape} in question. \emph{Rectangle} and \emph{Circle} augment their superclass with additional geometric data and implement the \emph{abstract} \emph{draw} method. To exercise subtype polymorphism different kinds of shapes are placed inside a collection containing shapes. The collection is then iterated over drawing each individual shape.

We first show the implementation of the `shapes` example in Java followed by an implementation in Haskell using our library.

\subsection{Shapes in Java}
\label{subsec:shapesjava}

The \emph{Shape} class can trivially be translated to Java:

\begin{verbatim}
public abstract class Shape {
  private int x;
  private int y;

  public Shape(int newx, int newy) {
    x = newx;
    y = newy;
  }

  public int getX() { return x; }
  public int getY() { return y; }
  public void setX(int newx) { x = newx; }
  public void setY(int newy) { y = newy; }

  public void moveTo(int newx, int newy) {
    x = newx;
    y = newy;
  }

  public void rMoveTo(int deltax, int deltay) {
    moveTo(getX() + deltaX, getY() + deltay);
  }

  public abstract void draw();
}
\end{verbatim}

The \emph{Shape} constructor receives an x and y coordinate of type \emph{int} and assigns them to its private member variables. The \emph{draw} method is marked as abstract. Subclasses stay abstract if they do not implement \emph{draw} or add new abstract methods. 

Here follows the definition of \emph{Rectangle}:

\begin{verbatim}
public class Rectangle extends Shape {
  
  // Private attributes
  private int width;
  private int height;

  // Constructor
  Rectangle(int newx, int newy, int newwidth, int newheight) {
    super(newx, newy);
    width  = newwidth;
    height = newheight;  
  }

  // Accessors
  public int getWidth() { return width; }
  public int getHeight() { return height; }
  public void setWidth(int newwidth) { width = newwidth; }
  public void setHeight(int newheight) { height = newheight; }

    // Implementation of the abstract draw method
  public void draw() {
    System.out.println(
         "Drawing a Rectangle at:("
      ++ getX() ++ "," ++ getY()
      ++ "), width " ++ getWidth() 
      ++ ", height " << getHeight()
    );
  }
}
\end{verbatim}

\emph{Circle} is defined similarly, we elide its full definition for the sake of brevity. 

\begin{verbatim}
public class Circle extends Shape {
  Circle(int newx, int newy, int newradius) {
    super(newx, newy);
    ...
  }
}
\end{verbatim}

Next, we put different kinds of shapes into a single collection of shapes. Inserting a \emph{Rectangle} and \emph{Circle} into a collection where shapes are expected exercises the language's ability to perform subtype polymorphism.

\begin{verbatim}
Shape[] scribble = new Shape[2];
scribble[0] = new Rectangle(10, 20, 5, 6);
scribble[1] = new Circle(15, 25, 8);
for(int i = 0; i < 2; i++) {
  scribble[i].draw();
  scribble[i].rMoveTo(100, 100);
  scribble[i].draw();
}
\end{verbatim}

We iterate over the list and draw the individual shapes to the screen.

\subsection{Shapes in Haskell}
\label{subsec:shapeshaskell}

We will now show how the shapes example is transcribed to Haskell using our library for OO programming. The library works with regular Haskell records, the |Data.Dynamic| library, and uses the C pre-processor (CPP) to derive some of the necessary boilerplate.

First, we transcribe the interface of the \emph{Shape} class as a Haskell record. Analogous to a Java interface.

\begin{code}
data IShape a = IShape {
      getX        :: IO Int
   ,  getY        :: IO Int
   ,  setX        :: Int -> IO ()
   ,  setY        :: Int -> IO ()
   ,  moveTo      :: Int -> Int -> IO ()
   ,  rMoveTo     :: Int -> Int -> IO ()
   ,  draw        :: IO ()
   ,  _shapeTail  :: Record a
}

-- Derive boilerplate 
DefineClass(Shape,IShape,shapeTail,,1)
\end{code}

Record selectors correspond to methods in the \emph{Shape} class. There is a \emph{single} special method |_shapeTail| for the extension of the record or as we shall call it \emph{the tail of the record}. It is an artifact of our dependence on regular Haskell records and cannot be abstracted over. The technique of leaving the tail polymorph is known as type extension through polymorphism \cite{Burton:1990:TET:77606.214515}. Finally, we use a CPP macro for deriving some of the boilerplate for manipulating records (see section \ref{sec:boilerplate}).

The implementation of \emph{Shape} is given by |shape| function:

\begin{code}
-- An implementation of the shapes interface
shape newx newy concreteDraw = clazz $ \tail self ->
  -- Create references for private state
  x <- newIORef newx
  y <- newIORef newy
  -- Return a Shape
  return IShape {
      getX       = readIORef x 
    , getY       = readIORef y
    , setX       = writeIORef x
    , setY       = writeIORef y
    , moveTo     = \newx newy -> do
        self # setX $ newx
        self # setY $ newy
    , rMoveTo    = \deltax deltay -> do
        x <- self # getX
        y <- self # getY
        (self # moveTo) (x + deltax) (y + deltay)
    , draw       = concreteDraw self
    , _shapeTail  = tail
  }
\end{code}

It takes the two initial values for |x| and |y|, an implementation for the |draw| method, an extension of the record, a self-reference, and returns an instance of \emph{Shape} (i.e. a value of |IShape|). The |concreteDraw| parameter makes it explicit that we cannot obtain an instance of \emph{Shape} unless we provide it with an implementation of \emph{draw}. Consequently, we can easily create instances of \emph{Shape} without creating a subclass -- analogous to an anonymous inner class in Java. The |clazz| combinator, only used for classes with no parent class, brings two additional parameters into scope |tail| and |self|. 

\begin{code}
clazz cont tail self = tail >>= \t -> cont t self
\end{code}

The |tail| parameter represents the extension of the record and should only be used at the tail position, |_shapeTail| in this case. Interestingly, |self| is an explicit parameter of the function whereas in most OO languages it is implemented as an language primitive. For stylistic purposes we use some syntactic sugar to distinguish between regular functions and methods:

\begin{code}
-- Reverse application
(#) :: a -> (a -> b) -> b
o # f = f o
\end{code}

The \emph{Rectangle} interface is transcribed similar to \emph{Shape}'s the only difference is that we use a different macro for deriving the boilerplate.

\begin{code}
data IRectangle a = IRectangle {
    _getWidth       :: IO Int
   ,_getHeight      :: IO Int
   ,_setWidth       :: Int -> IO ()
   ,_setHeight      :: Int -> IO ()
   ,_rectangleTail  :: Record a
}

-- Boilerplate for record manipulation and subtype axioms
DefineSubClass(Rectangle,Shape,IRectangle,rectangleTail,,,,1,)
\end{code}

Here follows the implementation of the \emph{Rectangle}:

\begin{code}
rectangle x y width height =
   -- Create a new object generator by connecting the records of shape and rectangle
   (rectangle' `extends` shape x y draw) noOverride set_Shape_Tail
   where
   rectangle' tail super self = do
      w <- newIORef width
      h <- newIORef height
      return IRectangle {
            _getWidth       = readIORef   w
         ,  _getHeight      = readIORef   h
         ,  _setWidth       = writeIORef  w 
         ,  _setHeight      = writeIORef  h
         ,  _rectangleTail  = tail
      }

   -- The implementation of the abstract draw method
   draw self = printLn (
        "Drawing a Rectangle at:("
    <<  self # getX 
    <<  ", " 
    <<  self # getY 
    <<  "), width "  
    <<  self # getWidth 
    <<  ", height " << self # getHeight
  )
\end{code}

We use the |extends| combinator in order to make \emph{Rectangle} a subclass of \emph{Shape}. It combines the implementation of \emph{Rectangle} given by |rectangle'| with that of its superclass. The right-hand side of |extends| is analogous to the call to super in the Java example. In between the extension of the superclass with its subclass there is an opportunity to override functionality defined in the superclass. The function that allows for this to happen is also passed as a parameter to |extends|. Because \emph{Rectangle} does not override any functionality we simply pass in |noOverride| which is essentially the identity function.

Notice how we left out the underscore prefix on the method invocations inside the |draw| implementation, because objects are represented as a nested records we need a helper method to invoke for instance |_getWidth|. The implementation of these helper functions corresponds to the top-down unwrapping of the record extensions until the target method is reached.

\begin{code}
-- Boilerplate for explicit method lookup
getWidth   =  _getWidth   .  unRecord  .  _shapeTail
getHeight  =  _getHeight  .  unRecord  .  _shapeTail

-- etc.
\end{code}

In an OO language the method lookup algorithm takes care of these method lookups starting at the callee tracing the pointers until the relevant method is found. Our method lookup works the other way around by starting at the top. 

We leave out the implementation of \emph{Circle} which is conceptually no different from \emph{Rectangle} and continue with the implementation of the scribble loop:

\begin{code}
myOOP = do

  s1 <- new $ rectangle 10 20 5 6
  s2 <- new $ circle 15 25 8

  -- Create a single homogeneous list of shapes 
  -- Shape is short for: IShape ()
  let  scribble  :: [IShape ()]
       scribble  = consUb s1 (consUb s2 nilUb)

  -- Iterate over the homogeneous list with a monadic version of map discarding the result
  sequence_ $ map  (\shape -> do 
                      shape # draw
                      (shape # rMoveTo) 100 100
                      shape # draw)
              scribble  
\end{code}

We use the |new| combinator to new create object instances. Unlike Java or any other OO language with subtype polymorphism we cannot simply place |s1 :: Rectangle| and |s2 :: Circle| inside a list of shapes, so we use a helper function |consUb| to automatically convert their types to \emph{Shape} before we cons them onto the list.

\begin{verbatim}
ghci> myOOP
Drawing a Rectangle at:(10, 20), width 5, height 6
Drawing a Rectangle at:(110, 120), width 5, height 6
Drawing a Circle at:(15,25), radius 8
Drawing a Circle at:(115,125), radius 8
\end{verbatim}

\section{Objects in Haskell}
\label{sec:tp-rec}

In this section we will incrementally develop the object encoding used by our library. Similar to \oohaskell we follow the examples from the OCaml tutorial \cite{ocaml}.

\subsection{Objects as tail-polymorphic records}
\label{subsec:tp-records}

\begin{quote}
"The class point below defines one instance variable varX and two methods getX and
moveX. The initial value of the instance variable is 0. The variable varX is declared mutable.
Hence, the method moveX can change its value.", \emph{section 3.1}
\end{quote}
\vspace{10pt}
\begin{OCaml}
class point =
  object
    val mutable varX = 0
    method getX = varX
    method moveX d = varX <- varX + d
end;;
\end{OCaml}

In a first attempt at transcribing the one-dimensional \emph{Point} class we map its interface to a record and let every method correspond to a selector.

\begin{code}
data Point = Point {
   getX      :: IO Int
  ,moveX     :: Int -> IO ()
}
\end{code}

The |point| function instantiates a \emph{Point} by creating a new value of type |Point|. It models the mutable variable \emph{varX} as an |IORef| lexically scoped over the record. Here objects are closures of records\footnote{Objects as closures in Scheme: \url{ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/swob.txt}}.

\begin{code}
point = do
  varX <- newIORef 0
  return Point {
     getX      = readIORef varX
    ,moveX     = \d -> modifyIORef varX ((+) d)
  }
\end{code}

A method is a function which works on and belongs to an object, i.e. it has access to the state encapsulated by the object. Using |point| we can write some basic OO code:

\begin{code}
myFirstOOP = do
   p <- point 
   p # getX >>= print
   p # moveX $ 3
   p # getX >>= print
\end{code}

\begin{verbatim}
> myFirstOOP
0
3
\end{verbatim}

There are a couple of problems with the encoding. In a typical OO language methods and data are for efficiency reasons modeled as separate entities such that methods can be shared among objects of the same type. Because efficiency is not our primary concern we stick with the conceptually simpler approach where data and methods are modeled as a single entity. Of a more pressing nature is the impossibility to extend objects with additional methods. For this reason, amongst other things, the authors of \oohaskell resort to using extensible records \cite{hlist}. Instead of lifting the Haskell 98 restriction we stick with regular records and use a poor man's approach to extensible records called type extension through polymorphism \cite{Burton:1990:TET:77606.214515}, which can easily be modeled using a parameterized record type:

\begin{code}
data Point alpha = Point {
  ...
  , _pointTail :: alpha
}
\end{code}

The |Point| type is modified to take a type parameter ($\alpha$) which represents the extension/tail of the record together with a special method for manipulating it. To account for the extension of |Point| the |point| function is also modified to take an additional parameter representing a computation that will result in the tail: 

\begin{code}
point tail = do
  ...
  record <- tail
  return Point {
       ...
    ,  _pointTail = record
  }
\end{code}

Now that we can extend |Point| with another record we also want to have a way of closing the record extension.

\begin{code}
emptyRecord :: IO ()
emptyRecord = return ()
\end{code}

The |emptyRecord| represents the end of a record extension. Like before we can construct a |point| object, but we now first apply it to the |emptyRecord|:

\begin{verbatim}
myFirstOOP = do
   p <- point emptyRecord
   ...
\end{verbatim}

We create an extension of |Point| called |Point2D| for representing points in the 2-dimensional plane.

\begin{code}
point2d tail =
  point point2d'
  where
  point2d' = do
    varY    <- newIORef 0
    record  <- tail
    return Point2D {
       _getY         = readIORef varY
      ,_moveY        = \d -> modifyIORef varY ((+) d)
      ,_point2DTail  = record
    }
\end{code}

We omit the interface definition of |Point2D| as its signature can be easily inferred from the implementation. The fact that we pass |point2d'| to |point| clearly expresses that a 2d-point is constructed out of a |Point| linked to a |Point2D|. This becomes even clearer when we look at the type:

\begin{verbatim}
>:t point2d emptyRecord
Point (Point2D ())
\end{verbatim}

The type structure revealed by our encoding is in fact equivalent to the phantom type structure used in \cite{Finne:1999:CHH:317765.317790} to model a type safe interface to external OO code, which was later formalized in\cite{Fluet:2006:PTS:1180085.1180088}. However, contrary to the phantom types our encoding has meaningful Haskell inhabitants.  

We have already encoded a small hierarchy of points. To accompany colored points we can simply extend the hierarchy:

\begin{tikzpicture}
[level distance=15mm,level/.style={sibling distance=40mm/#1}]
\node {Point ()}
  child {node {Point (Point2D ())}}
  child {node {Point (ColoredPoint ())}}
  child {node {...}};
\end{tikzpicture}

Every node in the hierarchy represents an unique object type, each child node has exactly one more nested record than its parent, and there are a finite number of object types. Under these conditions the hierarchy forms a finite subtype hierarchy where each child is in a subtype relationship with its parent. 

We can benefit from type unification to express the subtyping relationship to the type system:

\begin{code}
getX :: Point alpha -> IO Int
\end{code}

By leaving the tail of |Point| polymorph |getX| can be applied to any object type that is \emph{at least} a |Point|.
The |Point alpha| type is an abstract encoding matching a set of concrete encodings. The phantom type encoding allows abstract encodings to occur in the co-variant (producing) position \cite{Fluet:2006:PTS:1180085.1180088}, but because we always have values associated it is not safe to do so. Hence we limit abstract encodings to the contravariant (consuming) position. 

In the previous section we showed how method lookups are performed, but left their type unspecified. In our encoding a method always expects an abstract encoding as its first argument enabling reuse across all subtypes.

\begin{code}
-- Optional type
getY :: Point (Point2D a) -> IO Int
getY = _getY . _pointTail
\end{code}

To make our notion of interface, class, and object somewhat less vague we provide their definitions.

\vspace{10pt}
\begin{definition}
\label{def:interface}
An interface is a record $C$ with the following shape:

|data C t = C { m, _cTail :: t }|

where $C$ takes a type parameter $t$ representing the tail, and may be instantiated to either |()| or a interface. In the body $m$ expands to zero or more methods, and |_cTail| is a special method where |c| is the uncapitalized version of |C|.
\end{definition}

\vspace{10pt}
\begin{definition}
\label{def:class}
A class is a function that provides the implementation of an interface.
\end{definition}

\vspace{10pt}
\begin{definition}
\label{def:object}
An object is an instantiation (value) of an interface obtained through a class.
\end{definition}

Thus far we have explained the basic object encoding underlying our library together with its rational. Although we will later discover that we need to slightly modify it in order to facilitate casting the basic idea will remain the same.

\subsection{Constructor arguments}

\begin{quote}
"The class point can also be abstracted over the initial value of |varX|. The parameter |x_init| is, of course, visible in the whole body of the definition, including methods. For instance, the method |getOffset| in the class below returns the position of the object relative
to its initial position.", \emph{section 3.1}
\end{quote} 
\vspace{10pt}
\begin{OCaml}
class para_point x_init =
  object
    val mutable varX = x_init
    method getX      = varX
    method getOffset = varX - x_init
    method moveX d   = varX <- varX + d
end;;
\end{OCaml}

The previous incarnation of |point| allocated an initial value for \emph{varX} inside its body. There is nothing restriction us from moving the initial value out of the body and turning it into an argument. Here follows a more general version of |point|:

\begin{code}
para_point tail x_init = do
  record  <- tail
  varX    <- newIORef x_init
  return ParaPoint {
     getX            = readIORef varX
    ,moveX           = \d -> modifyIORef varX ((+) d)
    ,getOffset       = readIORef varX >>= \x -> return (x - x_init)
    ,_paraPointTail  = record
  }
\end{code}

\subsection{Construction-time computations}

\begin{quote}
"Expressions can be evaluated and bound before defining the object body of the class. This is useful to enforce invariants. For instance, points can be automatically adjusted to the nearest point on a grid, as follows:", \emph{section 3.1}
\end{quote}
\vspace{10pt}
\begin{OCaml}
class adjusted_point x_init =
   let origin = (x_init / 10) * 10 in
   object 
     val mutable varX   = origin
     method getX        = x
     method getOffset   = x - origin
     method moveX     d = x <- x + d
   end;;
\end{OCaml}

Similar to OCaml we may perform computations prior (in the non-strict sense) to object construction by using a let binding.

\begin{code}
adjusted_point tail x_init =
  let origin = (x_init / 10) * 10
  in do  ...
         varX <- newIORef x_init
         return ParaPoint {
            ...
           ,getOffset  = readIORef varX >>= \x -> return (x - origin)
           ...
         }
\end{code}

\subsection{Semi-explicit parameterized classes}
\label{subsec:semi-explicit-poly}

The |para_point| function has its argument type fixed to |Int|. This may proof unnecessarily restrictive. We can lift the restriction by introducing an additional type parameter to |ParaPoint|.

\begin{code}
data ParaPoint a t = ParaPoint {
   getX            :: IO a
  ,moveX           :: a -> IO ()
  ,getOffset       :: IO a
  ,_paraPointTail  :: t
}
\end{code}

The type inferencer will automatically infer a more general type for |para_point| without any modifications to |para_point|. 

\begin{code}
para_point :: Num a => a -> IO (ParaPoint a t)
\end{code}

Parameterized points are now bounded polymorph and can be constructed using \emph{any} type of number. 

\begin{code}
myPolyOOP = do
   p   <- para_point emptyRecord (1::Int)
   p'  <- para_point emptyRecord (1::Double)
   p # moveX $ 2
   p' # moveX $ 2.5
   p # getX >>= print
   p' # getX >>= print
\end{code}

If we were to apply methods of |p| with the wrong type

\begin{code}
-- Ill-typed
myPolyOOP = do
  ...
  p # moveX $ 2.5
  ...
\end{code}

we get a type error because the type checker expected the argument of |moveX| to have type |Int| but it got a |Double|. The generalization of classes to multiple type parameters is further explored in section \ref{subsec:paraclasses}. 

\subsection{Nested object generators}

\begin{quote}
"The evaluation of the body of a class only takes place at object creation time. Therefore,
in the following example, the instance variable |varX| is initialized to different values for
two different objects.", \emph{section 3.1}
\end{quote}

\begin{OCaml}
let x0 = ref 0;;
class incrementing_point :
  object
    val mutable varX = incr x0; !x0
    method getX      = varX
    method moveX d   = varX <- varX + d
end;;
\end{OCaml}

The variable |x0| mimics what would be referred to in OO terminology as a \emph{class variable}. The scope of a class variable is not limited to object instances, but as its name suggests ranges over all instances of a particular class. We could model |x0| as a global variable like in the fragment above. However, we can do much better by using what \oohaskell calls nested object generators.

\begin{code}
makeIncrementingPointClass = do
   x0    <- newIORef 0
   return $ \tail -> do
    record <- tail
    modifyIORef x0 (+1) 
    varX <- readIORef x0 >>= newIORef 
    return Point {
       getX       = readIORef varX
      ,moveX      = \d -> modifyIORef varX (+d)
      ,_pointTail = record
    }
\end{code}

The |makeIncrementingPointClass| consists of two levels: the outer level describes the \emph{class template}, the inner the point class. This is possible because there is nothing preventing us from returning classes instead of object instances. Classes are like objects just values unlike the case in many OO languages where they are special constructs.

\begin{code}
myNestedOOP = do
   localClass <- makeIncrementingPointClass
   localClass emptyRecord >>= (# getX) >>= print
   localClass emptyRecord >>= (# getX) >>= print
\end{code}

If we run |makeIncrementingPointClass| it returns a closure over |x0|. Hence, each time |localClass| is used to create a new instance of |Point| the construction-time computation increments the class variable |x0|.

\begin{verbatim}
ghci> myNestedOOP
1
2
\end{verbatim}

\subsection{Self-referential objects}
\label{subsec:self-ref}

\begin{quote}
"A method or an initializer can send messages to self (that is, the current object). For
that, self must be explicitly bound, here to the variable |s| (|s| could be any identifier, even
though we will often choose the name self.) ... Dynamically, the variable |s| is bound at
the invocation of a method. In particular, when the class printable\_point is inherited,
the variable s will be correctly bound to the object of the subclass.", \emph{section 3.3}
\end{quote}

\begin{OCaml}
class printable_point x_init =
  object (s)
    val mutable varX = x_init
    method getX      = varX
    method moveX d   = varX <- varX + d
    method print     = print_int (s # getX)
end;;
\end{OCaml}

Thus far we have avoided objects wherein methods refer to each other. The ability to refer to other methods inside the object is an essential feature of OO language and is enabled by a special identifier typically called \emph{this} or \emph{self}. The lack of such a special keyword in our encoding leaves us with the question on how we can provide a class with a reference to itself before it is even constructed? An imperative approach to solving this problem would be to use a mutable reference and leave it undefined to just after the object is constructed when it should be fixed with a proper reference to the object. All under the assumption that the self-reference is not touched during object construction as it would lead to undefined behavior. 

We could explicitly write down this process, but fortunately it has already been captured by the |fixIO| combinator \cite{Erkok01semanticsof}.

\begin{code}
fixIO :: (a -> IO a) -> IO a
\end{code}

It takes a function that takes as its first argument expects a value of the type that it itself produces. We will use |fixIO| to provide objects with a self-reference. Because object instantiation now consists of two separate actions -- closing record extension, and passing a self-reference -- we capture the act of creating a new object instance inside a combinator:

\begin{code}
new o = fixIO $ o emptyRecord
\end{code}

We implement the |printable_point| class and have it take a self-reference:

\begin{code}
printable_point x_init = clazz $ \tail self -> do
  varX    <- newIORef x_init
  return PrintablePoint {
     getX                 = readIORef varX
    ,moveX                = \d -> modifyIORef varX ((+) d)
    ,print                = (self # getX) >>= putStr . show
    ,_printablePointTail  = tail
  }
\end{code}

Note that we essentially rely on laziness for this construction to work, |self| should only be accessed in safe positions (inside methods) as premature evaluation would cause the program to crash. We can test the self-reference by invoking |print|.

\begin{code}
mySelfishOOP = do
   p <- new $ printable_point 3
   p # moveX $ 2
   p # print
\end{code}

In OO languages it is common practice to call initializer methods on an object inside the constructor. This can be mimicked by wrapping a class and carry out some initialization logic before returning the actual instance:

\begin{code}
printable_point_constructor x_init tail self = do
  p <- printable_point x_init tail self
  p # moveX $ 2
  p # print
  return p
\end{code}

\subsection{Single inheritance with override}

\begin{quote}
"We illustrate inheritance by defining a class of colored points that inherits from the
class of points. This class has all instance variables and all methods of class point, plus
a new instance variable color, and a new method getColor.", \emph{section 3.7}
\end{quote}
\vspace{10pt}
\begin{OCaml}
class colored_point x (color : string) =
  object
  inherit point x
  val color = color
  method getColor = color
end;;
\end{OCaml}

Inheritance is a technique for sharing behavior between objects. It accomplishes sharing by incrementally extending classes -- better known as subclassing. Often inheritance is confused with the orthogonal question of \emph{substitutability}. Creating a new subclass is not necessarily the same thing as introducing a new subtype \cite{Cook:1989:IS:96709.96721}. Although disparate issues our encoding does not permit the separation of the two, i.e. inheritance necessarily implies subtyping.

The correct implementation of inheritance in combination with self-reference is known to be tricky \cite{Cook89adenotational}. It is crucial that |self| is \emph{late-bound}, i.e. when a class is extended |self| is bound at the latest possible moment such that a subclass may intercept method invocations on |self| in the superclass by overriding its behavior in the subclass. Late-binding is also referred to as \emph{open recursion} conveying the intuition that the actual type of |self| is left \emph{open} until the \emph{recursion} is closed.

The |colored_point| class takes an initial values for |x|, |color|, and an extension of the record.

\begin{code}
colored_point x color = clazz $ \tail self ->
   printable_point x colored_point' 
   where
   colored_point' = do
      return ColoredPoint {
          _getColor          = return color
         ,_coloredPointTail  = tail
      }
\end{code}

Compared to our previous attempt at extending records in section \ref{subsec:tp-records} the object now has access to itself through |self|. Note that the self-reference should not be accessed in unsafe positions, i.e. positions where it is evaluated before the actual object is constructed and the self-reference is fixed. The combination of record extension  \ref{subsec:tp-records} and self-reference \ref{subsec:self-ref} allows us to model a basic form of inheritance:

\begin{code}
myColoredOOP = do
   p <- new $ colored_point 3 "red"
   x <- p # getX
   c <- p # getColor
   print (x, c)
\end{code}

\begin{verbatim}
> myColoredOOP
(3, "red")
\end{verbatim}

The above code shows that subclassing works, but we have yet to consider overriding methods. To show what is wrong with the approach to overriding methods shown in \oohaskell (section 2.4, p. 22) we adapt |colored_point| by overriding the |print| method.

\begin{code}
colored_point x color = clazz $ \tail self -> do
   super <- printable_point x colored_point' self 
   return super {
      print = do  putStr "so far - ";  super # print
                  putStr "color - " ;  putStr (show color)
   }

   where

   colored_point' = do
      return ColoredPoint {
          _getColor          = return color
         ,_coloredPointTail  = tail
      }
\end{code}

Overriding |print| is done by updating the record that results from constructing a |ColoredPoint|. There are, however, a couple of questionable aspects about this approach. First, referring to the whole record as |super| is not appropriate as |super| should only refer to the parent object. Second, as a side-effect of letting |super| refer to the whole object we cannot refer to |super| in |colored_point'|. Unsatisfied with this approach to overriding methods provided by \oohaskell we continue to explore what it takes to properly model inheritance.

\subsubsection{Deriving the inherit combinator}

One of the key observations in implementing inheritance is that |super| refers to the fully constructed parent object. It exists as such in the scope of the subclass, allowing methods to refer to the \emph{unmodified} parent object, after which it can be opened up to be extended with additional methods, possibly overriding methods of the |super| object. 

For |colored_point| this means that we first instantiate its parent |printable_point| with the |emptyRecord| which results in a binding to |super| that part-takes in the construction of |colored_point'|. Then the |print| method is overridden and the |emptyRecord| is replaced with the |ColoredPoint| extension containing all additional methods and data. 

\begin{code}
colored_point x color = clazz $ \tail self -> do
   super    <- printable_point x emptyRecord self
   wrapper  <- colored_point' tail super self
   return super {
      print = do  putStr "so far - "  ;  super # print
                  putStr "color - "   ;  putStr color
      ,_printablePointTail = wrapper
   }

   where

   colored_point' tail super self = do
      return ColoredPoint {
          _getColor          = 
            do  x <- super # getX 
                putStrLn ("Retrieving color at: " ++ show x) 
                return color
         ,_coloredPointTail  = tail
      }
\end{code}

Because we use normal records the process of overriding and extending becomes somewhat entangled. 

Cook et al. \cite{Cook89adenotational} show how inheritance can be modeled using a combinator $\rhd$ defined in the lambda calculus. They proof its correctness with respect to the operational semantics of a commonly used OO method-lookup algorithm. 
\[
\rhd : (Wrapper \times Generator) \rightarrow Generator
\]
\[
W \rhd G = \lambda self. (W(self) (G(self))) \oplus G(self)
\]

The combinator takes a wrapper, generator, and builds a new generator by distributing the self-reference to both the generator and wrapper. It passes a generator applied to self as super to the wrapper, and combines the two using $\oplus$ forming a new generator. That the two instances of the generator applied to self are shared is left implicit, but is made clear by the visualization in figure \ref{fig:inheritancewrapper}.

\begin{figure}[h]
\center
\includegraphics[scale=.3]{resources/inheritance.png}
\caption{A visualization of the inheritance combinator ($\rhd$) taken from \cite{Cook89adenotational}.}
\label{fig:inheritancewrapper}
\end{figure}

It turns out that |colored_point| is a concrete instantiation of Cook's $\rhd$ combinator obfuscated by technicalities caused by the use of records and IO monad. To make this correspondence clear we reify the combinator as the |extends| Haskell function, but first we define a couple of type synonyms that will make the type signature easier to digest:

\begin{code}
type Class        tail  self  o        = tail -> self -> o

type EmptyClass                        = IO    ()

type OpenClass    tail  self  o        = Class (IO tail)   self  (IO o)

type SuperClass   self  sup            = Class EmptyClass  self  (IO sup)

type SubClass     tail  sup   self  o  = tail -> super -> self -> IO sub
\end{code}

The |extends| combinator takes a few more parameters than $\rhd$. Since we cannot define a generic operation for record concatenation (as \oohaskell does) we parameterize over it using $\oplus$. Furthermore, for syntactic purposes we have not combined |override| and $\oplus$ into a single operation.

\begin{code}
extends ::
         SubClass    tail  sup   self sub    -- w
     ->  SuperClass  self  sup               -- g
     ->  (sup   -> self        -> IO sup')   -- override
     ->  (sup'  -> sub         -> o)         -- combine subclass and superclass
     ->  OpenClass   tail  self  o
extends w g override oplus = clazz $ \tail self -> do
  super    <- g emptyRecord self
  wrapper  <- w tail super self
  super'   <- override super self
  return $ super' `oplus` wrapper
\end{code}

We can now express |colored_point| in terms of |extends|:

\begin{code}
colored_point x color = 
   (wrapper `extends` printable_point x) override (\o v -> o { _printablePointTail = v })

   where

   override super self = return super {
      print = do  putStr "so far - "  ;  super # print
                  putStr "color - "   ;  putStr color
   }

   wrapper tail super self =
      return ColoredPointClass {
          _getColor = do  x <- super # getX
                          putStrLn ("Retrieving color at: " ++ show x) 
                          return color
         ,_coloredPointTail = tail
      }
\end{code}

and demonstrate it through a simple example:

\begin{code}
myOverridingOOP = do
   p <- new $ colored_point 3 "red"
   p # getColor
   p # print
\end{code}

\begin{verbatim}
>myOverridingOOP
Retrieving color at position: 3
so far - 3 color - "red"
\end{verbatim}

Sometimes you might want to override existing methods without adding new ones. This form of anonymous overriding is also possible:

\begin{code}
colored_point' x_init color tail self = do 
   super <- colored_point x_init color tail self
   return $ super { 
      print = putStr "I'm a colored point"
   }
\end{code}

\subsection{Class-polymorphic functionality}

Because classes are just values we can parameterize computations over classes.

\begin{code}
-- Optional type
myFirstClassOOP :: 
  Num a  =>  (a -> IO (PrintablePoint b -> ()) 
                -> PrintablePoint b 
                -> IO (PrintablePoint b)) 
         ->  IO ()
myFirstClassOOP point_class = do
   p <- new $ point_class 7
   p # moveX $ 35
   p # print
\end{code}

Any subclass of |PrintablePoint| may be passed into |myFirstClassOOP|.

\begin{verbatim}
ghci>myFirstClassOOP printable_point
42

ghci>myFirstClassOOP (flip colored_point' "red")
so far - 42 color - red
\end{verbatim}

\subsection{Orphan methods}

Orphan methods are methods which can be shared between classes without relying on inheritance -- a kind of horizontal reuse. 

\begin{code}
print_getX self = (self # getX) >>= Prelude.print
\end{code}

The |print_getX| function can be applied to any subclass of |PrintablePoint|. In \oohaskell its type would be much more granular and hence work for any class that supports the |getX| method. We can get some of the structural behavior of \oohaskell by introducing a type class per method and overload the method on its object type. 

\begin{code}
class HasGetX o where
   callGetX :: o -> IO Int

instance HasGetX (PrintablePoint t) where
   callGetX = getX

print_getX self = (self # callGetX) >>= Prelude.print   
\end{code}

On occasions this might be useful, but it requires significant boilerplate and quickly runs into ambiguity problems for methods with polymorphic arguments.

\section{A type-perspective}
\label{sec:ty-per}

\subsection{Explicit casting}
\label{subsec:casting}

Up to this point we have avoided the issue of ascribing a value a different type based on its relationship in the subtyping hierarchy. Type ascription in the presence of subtyping is commonly known as casting. Given a value of type |X|, ascribing it a supertype is referred to as an upcast, whereas ascribing it a subtype is referred to as a downcast. The former allows |X| to be \emph{viewed} as its supertype and can therefore be regarded as a form of abstraction or \emph{elimination}. The latter can be viewed as a form of \emph{introduction} and is arguably more involved since it needs to recover from potentially hidden information. 

We will focus on single inheritance where each subtype has a single supertype:

\begin{tikzpicture}
  \tikzstyle{all nodes}=[inner sep=4pt,font=\scriptsize,scale=2]
  \node            (t) at (1,2)  { ... };
  \node            (td) at (4,2) { supertype };
  \node            (x) at (1,1)  { X };
  \node            (xs1) at (0,0) { ... };
  \node            (xs2) at (1,0) { ... };
  \node            (xs3) at (2,0) { ... };
  \node            (xsd) at (4,0) { subtype };
  \draw[->,thick] (x) edge (t);
  \draw[<-,dashed,thick] (xs1) edge (x);
  \draw[<-,dashed,thick] (xs2) edge (x);
  \draw[<-,dashed,thick] (xs3) edge (x);
\end{tikzpicture}

To illustrate why we need casting we show a typical OO scenario where two objects of different types, related by subtyping, are placed inside a list containing only elements of their supertype. 

\begin{code}
let  rect    = ... :: IShape (IRectangle ())
     circle  = ... :: IShape (ICircle ())
\end{code}

Suppose that we insert both shapes into a list:

\begin{code}
[rect, circle] -- Type error
\end{code}

The result is a type error, because the two type element types do not unify. What we actually want is that the type system infers the more general type |IShape a|, but this would imply that it has some notion of subtyping and allow universal quantification over monomorphic values at the covariant position, neither of which are the case.

There are two options to make it work either change the type of list elements, or change the type of the elements inserted into the list. The former leads to more idiomatic Haskell and has two basic options: use |Either|, or an existential envelope.

\begin{code}
-- Either
[Left rect, Right circle] :: [Either (IShape (IRectangle ())) (IShape (ICircle ()))]
\end{code}

Using |Either| requires tagging |rect| and |circ| with respectively |Left| and |Right|. It is the simplest solution and leaves the original types intact. That the original types are kept intact is at the same time one of the problems. We can construct the list, but we cannot pass it to a function that expects a list of any shape. Also, when generalizing to $n$ distinct types operations like injection, projection, and mapping become more involved.

\begin{code}
-- Existential envelope
[rect, cirlce] :: [exists a. IShape a]
\end{code}

Using existentials we can take advantage of the tail-polymorphic structure and hide the tail of a record by existentially quantifying over it. Now we can simply insert any subtype of |IShape| inside a list without further ado. However, plain existentials cannot not recover from the existentially quantified information. In section \ref{subsec:selfret} we will see how in combination with explicit casting we can mitigate the loss of information. 

Neither approach is satisfactory. \oohaskell uses yet another approach to insert elements into the list where it circumvents unification problems by explicitely changing the type of list elements before they are inserted. Their type is modified by using a narrowing function that \emph{shops off} the tail.

The narrowing function for shapes is defined as follows:

\begin{code}
-- Specific narrowing function
up_shape  :: IShape a -> IShape ()
up_shape o = o { _shapeTail = () }
\end{code}

It takes \emph{at least} a shape, opens up the object, and throws out the tail replacing it with the |emptyRecord'|. We can use |up_shape| as a helper to insert |rect| and |circle| into a list of shapes. 

\begin{code}
[up_shape rect, up_shape circle] :: [IShape ()]
\end{code}

Even though it provides a working solution it appears to be even less useful compared to existentials. It accomplishes the same thing with more boilerplate for the necessarily type specific record manipulation, a run-time overhead for performing the actual record manipulation, and requires knowledge of the whereabouts of the narrowing functions. Furthermore, similar to existentials it does not admit downcasting because it simply throws out information that may later be required to perform a downcast. 

In the following sections we will improve upon the above techniques by implementing generic functions for up and downcasting. We use type classes for automatically generating the type conversion functions and dynamic typing for hiding the tail instead of deposing it. The use of dynamic types allows a downcast to reconstruct the original types.

\subsubsection{Generic upcast}
\label{subsub:upcast}

An upcast is a function that when given a source (subtype) and target (supertype) type provides unique directed-path through the subtyping hierarchy from source to target type composed of the smallest possible narrowing steps that allow it to move along the edges of the path. 

Although |up_shape| conceptually corresponds to such a path its implementation does not. In order to implement a generic upcast function we need a more compositional approach that is explicit about the constituents of the path.

Suppose we want to upcast a \emph{Cube} to a \emph{Shape} obtaining a more explicit version than:

\begin{code}
from_cube_to_shape = up_shape
\end{code}

requires that we follow the edges in figure \ref{fig:upcast-exp} from \emph{Cube} to \emph{Shape}. The labels on the edges are functions that allow us to transition from one vertex to the other.

\begin{code}
up_rectangle :: IShape (IRectangle ()) -> IShape ()
up_rectangle o = o { _shapeTail = () } 

up_circle :: IShape (ICircle ()) -> IShape ()
up_circle o = o { _shapeTail = () } 

up_cube :: IShape (IRectangle (ICube ())) -> IShape (IRectangle ())
up_cube o = o { _shapeTail = _shapeTail o { _rectangleTail = () } }
\end{code}

The explicit version of |from_cube_to_shape| mentioning all labels in the path is given by:

\begin{code}
from_cube_to_shape = up_rectangle . up_cube 
\end{code}

\begin{figure}
\begin{tikzpicture}[level distance=15mm,level/.style={sibling distance=40mm/#1}, font={\small}]
\node {IShape ()}
  child {
    node[] {... (IRectangle ())} {
      child {
        node {
          ... (ICube ()))
        }
        edge from parent[<-,thick]
          node [right] {up\_cube}
      } 
    } 
    edge from parent[<-,thick] 
      node [midway,left] {up\_rectangle}
  }
  child {
    node {... (ICircle ())} {
      child {
        node [below] {$\vdots$}
      }
    }
    edge from parent[<-]
      node [right] {up\_circle}
  }
  child {node[below] {$\vdots$}};
  \draw[<-] (-7,0) -- (-7,-4) node[midway,below,sloped] {up\_shape};
\end{tikzpicture}
\caption{The shapes subtyping hierarchy where the edges are annotated with narrowing functions.}
\label{fig:upcast-exp}
\end{figure}

Where |up_shape| allowed us to take a shortcut the explicit version does not. It requires $N$ record manipulations, where

$N = \dfrac{1}{2}(n_s - n_t)(n_t + n_s - 1)$

and $n_s$, $n_t$ are respectively the depth of the source and target type with $n \geq 1$. Thus instead of a single record manipulation with |up_shape| it requires 3 record manipulations. Clearly, the explicit approach is less efficient but the effect is somewhat mitigated by the fact that $n$ is bounded by the subtyping depth which in practice is quite limited.

Manually writing functions like |from_cube_to_shape| is tedious, not generic, and hence offers no benefits over just using |up_shape|. The possibility to explicitly specify the path only turns into a benefit when we can craft a function that will automatically create the path for us provided with a source and target type. This is exactly what we aim for. A generic upcast function should therefore: given a source and target type automatically create the path between source and target by decomposing it into individual narrowing steps.

Because the behavior of an upcast depends on both the source and target type we model it using a multi-parameter type class\footnote{We use |TypeOperators| solely for stylistic purposes.}:

\begin{code}
class alpha :<: beta where
  upcast :: alpha -> beta
\end{code}

The type class reads: if |alpha| is a \emph{subtype} of |beta|, there exists an |upcast| operation which can be used to cast a value of type |alpha| into value of type |beta|. 

In order for this proposition to hold we rely on the essential property that each subclass can be interpreted as a new subtype, and thus walking over the type structure implies walking over the subtyping hierarchy. 

\begin{figure}[h!]
    \begin{centering}
        $ [$\emph{reflexivity}$] $ \inference{}
        {
        A <: A
        }
        \\
        \vspace*{10pt}
        $ [$\emph{transitivity}$] $ \inference{
          A <: B &   B <: C
        }
        {
        A <: C
        }
    \caption{Typing rules for the subtyping relation.}
    \label{fig:subtyping-rules}
    \end{centering}
\end{figure}

The rules that decide whether two types are related by subtyping are shown in Figure \ref{fig:subtyping-rules}. It is well-known that these type rules do not directly translate into an algorithmic implementation because it is not clear when they should be applied. In other words, they are not \emph{syntax directed} (see Chapter 16 \cite{tpl}).

It turns out that if we limit the inhabitants of the |:<:| type class to first-order values with a tail-polymorphic structure we can obtain a syntax directed version of the typing rules. Instance declarations then correspond to the typing rules which through context reduction provide the compiler with evidence on whether two types are related by subtyping. 

We use the `shapes` example in a first attempt at translating the typing rules to instance declarations on a \emph{per type} basis, i.e. introducing new instance declaration for each new type. The reflexivity rule is trivially implemented by the identity function, because upcasting a value to itself has no effect.

\begin{code}
-- Reflexivity
instance IShape ()                      :<:  IShape ()                where 
  upcast = id
instance IShape (IRectangle ())         :<:  IShape (IRectangle ())   where 
  upcast = id
instance IShape (ICircle ())            :<:  IShape (ICircle ())      where 
  upcast = id 
\end{code}

Note that we need to lift a Haskell 98 restriction which requires the shape of an instance head to be of the form |C (T a1 ... an)|, where |C| is designates the class, |T| a data type constructor, and |a1, ..., an| a set of distinct type variables. We lift it to arbitrary nested types by enabling the |FlexibleInstances| extension.

Next, we implement transitivity for \emph{Rectangle} and \emph{Circle}.

\begin{code}
-- Transitivity
instance (IShape () :<: beta) => IShape (IRectangle ())  :<:  beta     where 
   upcast = upcast . up_rectangle

instance (IShape () :<: beta) => IShape (ICircle ())     :<:  beta     where 
   upcast = upcast . up_circle 
\end{code}

The transitivity instances embed a single narrowing step and delegate further work to other instance declarations. We can now perform a generic upcast without knowing about the specifics of narrowing functions. The solution also allows for easy extension to new object types by simply adding another instance for reflexivity and transitivity. 

To illustrate what happens at compile-time we let the system automatically derive |from_cube_to_shape| for us:

\begin{code}
let cube = ... :: IShape (IRectangle (ICube ()))
in upcast cube :: IShape ()
\end{code}

The compiler builds up a proof tree for each application of |upcast| proving that the source and target type are related by subtyping thereby also ruling out silly casts (i.e. casts between two unrelated types). Here is the proof tree derived by the context reduction machinery for the above example:

\begin{prooftree}
  \AxiomC{}
  \LeftLabel{\scriptsize [refl-shape]}
  \UnaryInfC{|IShape () :<: IShape ()|}
  \AxiomC{$\cdots$}
  \LeftLabel{\scriptsize [trans-rect]}
  \BinaryInfC{|IShape (IRectangle ()) :<: IShape ()|}
  \AxiomC{$\cdots$}
\LeftLabel{\scriptsize [trans-cube]}
\BinaryInfC{|upcast cube :: IShape ()|}
\end{prooftree}

The value produced by the proof should look familiar. It corresponds to the \newline |from_cube_to_shape| function except that at the end it includes the identify function as a residue of the recursion.

\begin{spec}
    id . up_rectangle . up_cube

equiv  {- left-identity -}

    up_rectangle . up_cube

equiv  {- by definition -}

    from_cube_to_shape
\end{spec}

There are quite some subtleties involved in the transitivity case. First, we need two additional language extensions |FlexibleContexts| and |OverlappingInstances|. The former does the same for the context as |FlexibleInstances| does for the head. The latter is necessary because |beta| makes the transitive and reflexive case overlap. Fortunately, this is a harmless case of overlapping instances as it does not lead to any difficulties in determining the most specific instance which can still be determined by solely looking at the instance head. Leaving |beta| polymorph allows the recursive call to |upcast| to pick either another transitive instance or bottom out the recursion at the reflexive instance. This is necessarily the case because the choice on which instance to pick is determined by the instantiation of |beta| at the call site.

\emph{Pushing it a bit further}

While the \emph{instance per new subclass/subtype} approach works its dependence on concrete types requires an unnecessary amount of instances for classes with essentially the same type \emph{structure} (see \emph{Circle} and \emph{Rectangle}). A much better approach would be to abstract over the concrete types. As a consequence the problem of providing instance declarations shifts from \emph{per new subclass} to \emph{per increase in the depth of the subtyping hierarchy}. Fortunately, this is not that much of a problem because the depth of a subtyping hierarchy is in practice often quite limited.

We refactor the previous instance declarations, replacing the concrete types with type variables, but leaving the structure intact. 

\begin{code}
-- depth 1: reflexivity
instance c () :<: c () where
  upcast = id
\end{code}

For each subsequent increase in depth we add an instance for both reflexivity and transitivity.

\begin{code}
-- depth 2
instance a (b ()) :<: a (b ()) where
   upcast = id

instance (a () :<: x) => a (b ()) :<: x where
   upcast = upcast . (? :: a (b ()) -> a ())

-- depth 3
instance a (b (c ())) :<: a (b (c ())) where
   upcast = id

instance (a (b ()) :<: x) => a (b (c ())) :<: x where
   upcast = upcast . (? :: a (b (c ())) -> a (b ()))
\end{code}

By abstracting over the concrete types we gained some expressive power, but lost some information. We no longer know which narrowing functions should be called at the position of the question mark. In order to capture the specific narrowing functions we introduce a new type class |Narrow|. It allows us to defer the decision on what concrete narrowing function to use to the call site.

The |Narrow| type class captures the set of specific narrowing functions.

\begin{code}
class Narrow alpha beta where
  narrow :: alpha -> beta
\end{code}

We augment the transitivity instances with an additional |Narrow| constraint, and replace the question marks with calls to the |narrow| function. 

\begin{code}
-- Depth 2: add a Narrow constraint
instance (a () :<: x, Narrow (a (b ())) (a ())) => a (b ()) :<: x where
   upcast = upcast . (narrow :: a (b ()) -> a ())

-- From rectangle to shape
instance Narrow (IShape (IRectangle ())) (IShape ()) where 
   narrow = up_rectangle
\end{code}

It is important that the type variables in the head of the transitivity instance match those annotating the |narrow| function. We use the |ScopedTypeVariables| extension to bring the type variables in the head into scope such that they may be reused in the body of |upcast|. Furthermore, by adding the |Narrow| constraint the Paterson Conditions no longer hold, i.e. the context is no longer smaller than the instance head\footnote{For more details see: \url{http://www.haskell.org/ghc/docs/7.2.2/html/users_guide/type-class-extensions.html\#instance-rules}}. To lift this restriction we must enable |UndecidableInstances| with which termination of context reduction process is no longer guaranteed. Fortunately, this causes no problems because we can tell by our instance definitions that there is no possibility to send the context reduction into an infinite loop. 

The subtype type class has some interesting behavior, different from what one might intuitively expect from subtyping:

\begin{code}
foo c =
  let  c'  = upcast c :: A (B (C ()))
       b   = upcast c :: A (B ())
       a   = upcast c :: A ()
  in ()
\end{code}

On basis of subtyping one might expect the type of |foo| to correspond to: 

|foo :: (a :<: A (B (C ()))) => a -> ()|

since both |b| and |a| are included in |A (B (C ()))|. However, the actual type of |foo| corresponds to:

|foo :: (a :<: A (), a :<: A (B ()), a :<: A (B (C ()))) => a -> ()|

Each call to |upcast| contributes a constraint to the context. However, the context reduction machinery is not aware of the subtyping rules and therefore cannot reduce the constraint set to |a :<: A (B (C ()))|. This is not erroneous nor does it affect the behavior of |foo|, but for a clean type signature we must perform the context reduction in our head and provide |foo| with an explicit type signature.

\subsubsection{Generic downcast}
\label{subsec:down}

A downcast is the partial inverse of upcast. Its partial because it \emph{attempts} to restore type information whilst admitting the possibility of failure in case the expected type does not correspond to the actual type.

The problem with the current implementation of |upcast| is that the individual narrowing steps throw away the tail making it impossible to later restore it with a downcast. In order to facilitate downcasts the tail needs to be somehow maintained without having it surface in the type. A known technique for hiding types is existential quantification. However, using plain existentials we cannot recover from the hidden information. The |Data.Dynamic| library does allow hidden types to be restored by testing on type equality between the run-time type representations of the hidden value and its expected type. If the value's type matches the expected type it provides proof that the hidden value may safely be recovered from as the expected type.

Before implementing downcast we fix the tail representation by wrapping it inside a new data type |Record| such that an upcast can hide the tail.

\begin{code}
-- A wrapper for the tail
type Record alpha = Either alpha Dynamic
\end{code}

|Record| is represented using the binary sum type |Either a b|. With the tail wrapped inside a |Record alpha| value we gain the possibility of hiding the tail. We make no distinction between the type of an empty record and that of a hidden tail.

\begin{code}
-- Smart constructors
record = Left
unRecord (Left a) = a

hideRecord :: Typeable alpha => Record alpha -> Record ()
hideRecord (Left a) = Right (toDyn a)

emptyRecord :: IO (Record ())
emptyRecord = return (record ())

\end{code}

The |record| and |unRecord| functions wrap and unwrap a record, |hideRecord| hides a record by injecting it into a |Dynamic|, and |emptyRecord| is adapted to the new record representation. Note that the representation requires every interface to be an instance of |Typeable| such that it can be used by the |Data.Dynamic| library. 

We now wrap every tail inside a |Record| like this

\begin{code}
data IShape alpha = IShape { 
  ...
  ,_shapeTail :: Record alpha
}
\end{code}

, and revise the narrowing functions to use |hideRecord|:

\begin{code}
instance Narrow (IShape (IRectangle ())) (IShape ()) where
   narrow o = o { _shapeTail = hideRecord (_shapeTail o) }
\end{code}

The effect of the new representation is that for example an upcast from \emph{Cube} to \emph{Shape} still has the same type, but internally looks more like this:

IShape ({\color{gray}IRectangle (ICube ())})

where black is the visible and gray the invisible part of the type. By changing the representation we introduced the possibility of restoring the subtypes.

With the correct data structure in place we are ready to implement the downcast function. Since we do not know what types are possibly hidden -- it can be any subtypes -- a downcast is necessarily a partial function. The best we can do is \emph{try} to perform a downcast. A downcast is a function from a value of type |beta| to a value of type |Maybe alpha| which succeeds only if |beta| is a supertype of |alpha|, and |beta| can be converted into a value of type |alpha|. 

\begin{code}
downcast :: beta -> Maybe alpha
\end{code}

At first, it seems to make sense to place |downcast| inside the |:<:| type class given that |downcast| is the partial inverse of |upcast| and the instances are governed by the same subtyping rules.

\begin{code}
class alpha :<: beta where
  upcast    :: alpha -> beta
  downcast  :: beta -> Maybe alpha
\end{code}

This works fine for the reflexivity instances, but unfortunately breaks for transitivity as the type class constraints necessary for |downcast| are not incompatible with those required for |upcast|. An |upcast| forgets information while |downcast| tries to gain information. Hence, we introduce a separate type class for the supertype relation and rely on the library designer to ensure consistency between the two.

\begin{code}
-- New type class for downcasting
class alpha :>: beta where
  downcast :: alpha -> Maybe beta
\end{code}

For reflexivity a downcast always succeeds.

\begin{code}
-- Depth 1
instance a () :>: a () where
  downcast = Just

instance a (b ()) :>: a (b ()) where
  downcast = Just
\end{code}

The interesting case is transitivity.

\begin{code}
instance (a (b ()) :>: a (b c)) => a () :>: a (b c) where
  downcast o = case  ?  :: Maybe (a (b ())) of
                Just r   -> downcast r 
                Nothing  -> Nothing
\end{code}

The instance declaration should be read as follows: given a value of type |a ()| we may downcast it to a subtype |a (b c)| provided that we can downcast it to |a (b ())|, if the downcast is successful we delegate the task of resolving |c| to another instance, otherwise we fail by returning |Nothing|.

Similar to |upcast| we have lost information by abstracting over the actual types. We capture the dual of |Narrow| through a new type class:

\begin{code}
class Widen beta alpha where
  widen :: beta -> alpha
\end{code}

and replace the question mark with a call to |widen|:

\begin{code}
instance (a (b ()) :>: a (b c), Widen (a ()) (a (b ()))) => a () :>: a (b c) where
  downcast o = case widen o :: Maybe (a (b ())) of
                Just r   -> downcast r 
                Nothing  -> Nothing
\end{code}

By pattern matching on a known part of the type structure and an unknown part |c| a downcast can incrementally recover from the hidden types. The implementation of |widen| only works correctly if different parts of the library maintain the following invariants:
\begin{enumerate}
  \item An |upcast| always hides the tail of a record by using |hideRecord|.
  \item All interfaces should derive from |Typeable|.
  \item Fresh object instantiations always have |emptyRecord'| as tail.
  \item A downcast can only encounter a |Right| data constructor.
\end{enumerate}

Invariant (1) is not enforced by the type system. Without precautions it is possible for |upcast| to choose |emptyRecord'| which would destroy the invariant. Fortunately, as library designer we may export |Record| as an abstract data type and confine |emptyRecord| to internal use.

Invariant (2) is taken care of by the macros (see section \ref{sec:boilerplate}). However, if a interface does not derive from |Typeable| and (1) holds, using casting operations on that class will fail at compile-time. 

Invariant (3) is covered by the |new| combinator.

Invariant (4) follows from (1) and (3). A downcast cannot encounter |Left| because widening only starts at a concrete object type. Furthermore, it cannot encounter the empty record because any downcast always bottoms out at a reflexivity instance which itself does not inspect the representation.

All instances of |Widen| are defined in terms of |genericWiden|:

\begin{code}
instance Widen (IShape ()) (IShape (IRectangle ())) where
   widen o = genericWiden o _shapeTail (\o v -> o { _shapeTail = v })
\end{code}

The |genericWiden| function is passed the object it should perform widening on and a getter/setter for the object's tail. It discriminates on the object's tail and attempts to recover its tail.

\begin{code}
genericWiden :: forall o a b c. Typeable b => 
      o
  ->  (o -> Record a) 
  ->  (o -> Record b -> c) 
  ->  Maybe c
genericWiden o getTail setTail =
  case getTail o of
    Right d  -> maybe Nothing (Just . setTail o . record) (fromDynamic d :: Maybe b)
    Left  _  -> error "invariant (4)"
\end{code}


\subsubsection{A change in semantics}
\label{subsubsec:changeinsemantics}

In section \ref{subsec:tp-records} we informally described the semantics of tail-polymorphic records. The meaning of the tail-polymorphic type structure now slightly changes because we modified the object representation such that the tail of a record may be hidden using a dynamic type. Whereas an object of type |A ()| used to mean \emph{exactly} |A ()| it can now mean \emph{at least} |A ()| even though its type has not changed. Given that now both |A ()| and |A a| can be interpreted as \emph{at least} |A ()| we wonder: can we substitute one for the other? We cover this question of substitutability for the contra- and covariant position.

\begin{code}
  A () -> ... 
-- subst
  A a  -> ...
\end{code}

We may substitute |A ()| with |A a| in the contravariant position by changing the type signature. All calls to the function
can remain unchanged. However, from a implementation perspective this substitution may prove problematic since we cannot use casting on polymorphic objects. Also, the substitution transitively inhibits other functions from using casts. A better alternative would be to substitude |A ()| with a type variable constrained by the subtype type class:

\begin{code}
  A () -> ...
-- subst
  a :<: A () => a -> ...
\end{code}

Now the function can get any monomorphic subtype of |A ()| as argument, i.e. excluding |A a| or any polymorphic subtype (e.g. |A (B a)|). Furthermore, before the argument can be used it must first be casted to |A ()|.

We may substitute |A a| for |A ()| in the contravariant position by again simply changing the type signature:

\begin{code}
  A a -> ...
-- subst
  A () -> ...
\end{code}

This change will require all call sites to upcast the function argument to |A ()| unless the argument is already of type |A ()|. The function implementation will remain unaffected by the change.

We now consider substitutability in the covariant position. Substituting |A ()| with |A a| in the covariant position is not possible because this would require universal quantification over a value with a monomorphic type. However, what we can do is abstract over its tail using existential quantification:

\begin{code}
  ... -> A ()
-- subst 
  ... -> exists a. A a
\end{code}

The resulting existential type can then be freely applied to functions expecting an universally quantified value of type |A a|. Thus the existential gives us the same behavior we would otherwise get from returning a value of type |A a|.

If a function has |A a| in the covariant position it can only have gotten it as an argument. It can never by itself produce a value of type |A a|. If a function has |A a| in the covariant position we can substitute it with |A ()| provided that we modify the calling and return context to cast the argument and return value to the appropriate type.

We conclude that moving between |A a| and |A ()| is a delicate business. It does not always go without affecting the implementation, and in particular substituting |A ()| with |A a| may break functions that depend on casting. Using polymorphic objects transitively inhibits other functions from using casts, and when used as argument to a method requires rank-2 polymorphism. The only benefit they bring is that no type conversions are necessary. Hence from a user's perspective it makes sense to stick with monomorphic objects at the cost of some more explicit type conversions.

\subsubsection{Combinators}

Using the subtype type class we can define two asymmetric combinators |consUb| and |nilUb| that allow the construction of a homogeneous list out of object values related by subtyping. Upon inserting a value it is first cast to the supertype that is provided at the call site and then "consed" on to the list. The supertype should be the common upper bound of all elements in the list. 

\begin{code}
consUb :: forall a b. (a :<: b, Typeable a) => a -> [b] -> [b]
consUb o xs = (upcast o :: b) : xs 

nilUp = []
\end{code}

We have used the above combinators in section \ref{subsec:shapeshaskell} to insert different kind of shapes into a homogeneous list of shapes:

\begin{code}
let  scribble  :: [IShape ()]
     scribble  = consUb s1 (consUb s2 nilUb)
\end{code}

The dual, for the \emph{lower bound}, is implemented similarly:

\begin{code}
consLb :: forall a b. (b :>: a, Typeable b) => b -> [a] -> [a]
consLb o xs = 
  case downcast o :: Maybe a of
    Just x   -> x : xs
    Nothing  -> xs

nilLb = []
\end{code}

The definition of |consUb|/|nilUb| and |consLb|/|nilLb| only work for lists. We generalize their definition by overloading them on the container type such that they may be used for any container type that allows incremental construction.\footnote{|mempty|: mempty, |`mappend`|: mappend}

\begin{code}
class Applicative f => CastCons f where
   consUb :: forall a b. (a :<: b, Typeable b, Monoid (f b)) => a -> f b -> f b
   consUb o xs = pure (upcast o :: b) `mappend` xs

   consLb :: forall b a. (b :>: a, Typeable b, Monoid (f a)) => b -> f a -> f a
   consLb o xs = maybe xs (xs `mappend` . pure) (downcast o :: Maybe a)

   nilUb,nilLb  :: Monoid (f a) => f a 
   nilUb = mempty
   nilLb = mempty
\end{code}

Similarly, we can lift casting operations to work on container types:

\begin{code}
class Functor f => Castable f where
   fup :: a :<: b => f a -> f b
   fup = fmap upcast

   fdown :: forall a b. (Foldable f, Applicative f, Monoid (f a), b :>: a) => f b -> f a
   fdown = foldr (`mappend` . maybe mempty pure . (downcast :: b -> Maybe a)) mempty
\end{code}

Not all types that allow mapping also allow deconstruction, hence the |Applicative| constraint is pushed down as a function constraint. We use |fdown| to create a list of rectangles out of a list of shapes:

\begin{code}
let  scribble' :: [IShape (IRectangle ())]
     scribble' = fdown scribble 
\end{code}

We can also define the familiar |instanceof| operations in terms of |downcast|. It test if a value is of a particular type:

\begin{code}
instanceof :: forall a b. (b :>: a) => b -> a -> Bool
instanceof b _ = isJust (downcast b :: Maybe a)
\end{code}

Using |instanceof| we can define a function |selectiveDraw| that accepts any Shape, but only draws rectangles. 

\begin{code}
selectiveDraw :: IShape () -> IO ()
selectiveDraw shape = 
  when  (shape `instanceof` (undefined :: IShape (IRectangle ()))) 
        (shape # draw)
\end{code}

Note that |instanceof| only type checks if it is used for testing if an object actually is some subtype of its current type. There is no need for testing if it is an instance of some supertype because this is intrinsically known.

\subsection{Self-returning methods}
\label{subsec:selfret}

A self-returning method is a method whose return type is the type of self or some other type based on self. It is known that encapsulation by \emph{procedural data abstraction} requires recursive types for the precise typing of self-returning methods in the presence of inheritance (section 3.1 \cite{Cook:1989:IS:96709.96721}). On the left in figure \ref{fig:javaself} the problem is made explicit in Java (which lacks recursive types). To let the program type check some otherwise superfluous casts must be inserted. On the right there is a hack which uses Java Generics \cite{naftalin2006java} and a special \emph{getThis} function to reify the lost type information \footnote{\url{http://www.angelikalanger.com/GenericsFAQ/FAQSections/ProgrammingIdioms.html\#FAQ205}}.

\begin{figure}[h]
\center
\begin{tabular}{ p{5cm} || p{5cm} }
  \begin{verbatim}
class A {
  public A foo() {
    return this;
  }
}

class B extends A {
  public B bar() {
    return this;
  }
}

B b = new B();
// type check error
b.bar().foo().bar();
// fine
((B) b.bar().foo()).bar();
  \end{verbatim}
  & 
  \begin{verbatim}
abstract class A<T extends A<T>> {
  public T foo() {
    return (T) getThis();
  }
  public abstract T getThis();
}

class B extends A<B> {
  public B bar() {
    return this;
  }
  public B getThis() {
    return this;
  }
}

B b = new B();
// fine
b.bar().foo().bar();
  \end{verbatim}
\end{tabular}
\caption{On the left: an example in Java where we use self-returning methods in combination with inheritance. On the right: a trick to resolve the need for casting.}
\label{fig:javaself}
\end{figure}

In a first attempt to implement a self-returning method we reuse the shapes example and augment |IShape| with a method |meShape| that returns itself.

\begin{code}
data IShape alpha = IShape {
   ...
   meShape     :: IO (IShape alpha)
  ,_shapeTail  :: Record alpha
}
\end{code}

In the return type of |meShape| we simply refer to itself. Unfortunately, because |alpha| is now used at two positions in interface the |_shapeTail| function becomes less general compared to what it would have been had we only used |alpha| in a single position.

\begin{code}
-- Without meShape
_shapeTail :: IShape alpha -> Record beta -> IShape beta

-- With meShape
_shapeTail :: IShape alpha -> Record alpha -> IShape alpha
\end{code}

As a consequence we can no longer change the type of the tail in isolation making record extension highly impractical and impossible to fit into our framework for casting. Alternatively we could try to abstract over the return type by parameterizing over it, similar to what is done on the right in figure \ref{fig:javaself}. However, this will also not work because the type parameter now has to unify with itself which is prohibited by the occurs check. Because Haskell does have iso-recursive types \oohaskell uses newtype wrappers to solve this problem. Unfortunately, wrapping self inside a newtype will not work for our encoding as it again requires using the tail type parameter at multiple positions preventing record extension. 

Confronted with the impossibilities of the encoding we resort to a less sophisticated version of self-returning methods and take for granted that some casts are required. We start by making the return type of |meShape| concrete:

\begin{code}
data IShape alpha = IShape {
    ...
    meShape     :: IO (IShape ())
    ...
}

shape newx newy concreteDraw = clazz $ \tail self ->
  ...
  return IShape {
    ...
    meShape = return self
    ...
  }
\end{code}

The program type checks, but there is a subtle problem. The type inferencer has instantiated |self| to |IShape ()| which again inhibits further extension. We should somehow convince the type inferencer that it may temporarily assume |self| to be of type |IShape ()| without this knowledge overspecializing the inferred function type. There are two approaches that may help us achieve our goal: upcasting or existential quantification.  

\begin{code}
-- Upcast
meShape = return (upcast self :: IShape ())
\end{code}

By using |upcast| we make our knowledge that |self| is of at least |IShape ()| explicit changing the inferred type to:

\begin{code}
-- Inferred type
shape
  :: IShape a1 :<: IShape () =>
     Int
     -> Int
     -> (IShape a1 -> IO ())
     -> IO (IShape a1 -> Record a)
     -> IShape a1
     -> IO (IShape a)
\end{code}

Self is now overloaded by what at first sight may seem like a problematic constraint. However, because objects are always instantiated by using |new| we know that |a1| will eventually be instantiated to a concrete type such that substituting it in |IShape a1 :<: IShape ()| will satisfy the constraint.

As an example we show how we can call |meShape| on a \emph{Rectangle}. Calling |meShape| on a \emph{Rectangle} returns a \emph{Shape} which we downcast back to a \emph{Rectangle} and use to invoke |getWidth|:

\begin{code}
-- ((Rectangle) s1.meShape()).getWidth()
mySelfReturn = do
  s1     <- new $ rectangle 10 20 5 6
  shape  <- s1 # meShape
  let Just rect = downcast shape :: Maybe (IShape (IRectangle ()))
  w <- rect # getWidth 
  putStrLn $ show w
\end{code}

A different approach would be to existentially quantify over the tail of the record.

\begin{code}
-- Existential quantification
data IShape alpha = IShape {
    ...
    meShape :: exists alpha. IO (IShape alpha)
    ...
}
\end{code}

This makes it easy to return |self|.

\begin{code}
meShape = return self
\end{code}

But because the tail is existentially quantified over it does not allow casting. We solve this by placing a subtype constraint on the quantified variable.

\begin{code}
meShape :: exists alpha. (IShape alpha :<: IShape ()) => IO (IShape alpha)
\end{code}

Herewith the concrete type can be reified through an upcast.

\begin{code}
mySelfReturn = do
  ...
  let shape'     = upcast shape :: IShape ()
  let Just rect  = downcast shape' :: Maybe (IShape (IRectangle ()))
  ...
\end{code}

We gained ease of expression at the return site, but at the same time made the task of the caller more verbose. Also, the use of anonymous existentials is unique to UHC and requires newtype wrappers in GHC which makes it a far less attractive option. For these reasons we prefer to use concrete object types, even though the combination of top-level class definitions, overloading, and parameter hiding can trigger the monomorphism restriction\footnote{\url{http://www.haskell.org/onlinereport/decls.html\#sect4.5.5}}. 

\subsection{Parameterized classes}
\label{subsec:paraclasses}

In section \ref{subsec:semi-explicit-poly} we showed how we could create polymorphic classes by adding type parameters to the interface definition. Parameterization over method types allows for much greater flexibility because the same interface can be reused for instantiations with different concrete types. This is especially useful for container-like classes where operations on the container are independent of the actual contents. In this section we will further explore parameterized classes in combination with inheritance and casting. But before we do we first generalize our previous definition of an \emph{interface}.

\vspace{10pt}
\begin{definition}
An interface is a record $C$ with the following shape:

|data C a1, ..., an, t = C { m, _cTail :: Record t }|

where $C$ takes $n \geq 1$ type parameters, $t$ represents the tail, and may be instantiated to either |()| or an interface. In the body there is $m$ which expands to zero or more methods that may use any of $a_1,...,a_n$, and a special method |_cTail| where |c| is the uncapitalized version of |C|.
\end{definition}

Many statically typed OO languages have the ability to parameterize classes. We show that our library can easily deal with parameterized classes that are invariant in their type parameters. As a reference we have implemented a class \emph{Pair} in Java using generics, reminiscent of the Haskell tuple, and let another class \emph{Triple} extend from \emph{Pair} (see figure \ref{fig:javagenerics}). Both \emph{Pair} and \emph{Triple} are parameterized over their contained types.

\begin{figure}
\center
\begin{tabular}{ p{5cm} || p{5cm} }
  \begin{verbatim}
class Pair<A,B> {
   private final A a;
   private final B b;
   
   public Pair(A a, B b) {
      this.a = a;
      this.b = b;
   }

   public A getFirst() {
      return a;
   }

   public B getSecond() {
      return b;
   }
}
  \end{verbatim}
  & 
  \begin{verbatim}
class Triple<A,B,C> extends Pair<A,B> {
   private final C c;
   
   public Triple(A a, B b, C c) {
      super(a,b);
      this.c = c;
   }

   public C getThird() {
      return c;
   }

   public Triple<B,A,C> swap() {
      return 
        new Triple<B,A,C>(
          getSecond(),getFirst(),c
        );
   }
}
  \end{verbatim}
\end{tabular}
\caption{Two generic container types reminiscent of the Haskell tuple.}
\label{fig:javagenerics}
\end{figure}

We perform a stepwise transcription of the Java code to Haskell. First, the \emph{Pair} interface.

\begin{code}
data IPair a b t = IPair {
    _getFirst    :: IO a
  , _getSecond   :: IO b
  , _pairTail    :: Record t
}
\end{code}

In Java all classes are subclasses from \emph{Object}. Staying true to the example we also extend from \emph{Object} which we take to be a simple placeholder.

\begin{code}
pair a b = 
   (pair' `extends` object) noOverride set_Object_Tail
   where
   pair' tail super self = 
      return IPair {
          _getFirst   = return a
         ,_getSecond  = return b
         ,_pairTail   = tail
      }
\end{code}

For casting to work we provide the necessary instances for |Narrow| and |Widen|:

\begin{code}
type Pair_  a b t  = Object_ (IPair a b t)
type Pair   a b    = Pair_ a b ()

instance (Typeable a, Typeable b) => Narrow (Pair a b) Object where
   narrow = modify_Object_Tail hideRecord

instance (Typeable a, Typeable b) => Widen Object (Pair a b) where
   widen o = genericWiden o get_Object_Tail set_Object_Tail
\end{code}

Notice that we take fruitful use of the fact that the tail is always the last type parameter. If this were not the case we would have been be forced to write down all instances of the sub- and super type classes, for all interface shapes, which would lead to a combinatorial explosion in the number of instances.

We proceed by transcribing the |Triple| class. Interestingly, the |Triple_| type synonym and the Java class declaration look very much alike. In Java the type variables are introduced implicitly by usage whereas in Haskell they need to be explicitly declared before they can be used. In both cases the programmer is responsible for the correctly distributing the type variables.

\begin{code}
type Triple   a  b  c     = Triple_ a b c ()
type Triple_  a  b  c  t  = Pair_ a b (ITriple a b c t)

data ITriple a b c t = ITriple {
    _getThird     :: IO c
   ,_swap         :: IO (Triple b a c)
   ,_tripleTail   :: Record t
}
\end{code}

The implementation follows naturally from the interface definition. Unfortunately, the type inferencer does not infer the correct type for |triple|. It infers that both |a| and |b| should be of the same type because they are used interchangeably at different points in the program (see |_swap| and |pair|). We have to explicitely mark them as distinct by providing a type signature. 

\begin{code}
triple ::    
         a
     ->  b
     ->  c
     -> OpenClass (Record tail) self (Pair_ a b (ITriplet a b c tail)) 
triple a b c = 
   (triple' `extends` pair a b) noOverride set_Pair_Tail
   where
   triple' tail super self = 
      return ITriple {
          _getThird    = return c
         ,_swap        = new $ triple b a c
         ,_tripleTail  = tail
      }

swapTriple = _swap . unRecord . get_Pair_Tail
\end{code}

We also require two additional instances for |Narrow| and |Widen|:

\begin{code}
instance (Typeable a, Typeable b, Typeable c) => Narrow (Triple a b c) (Pair a b) where
   narrow = modify_Pair_Tail hideRecord

instance (Typeable a, Typeable b, Typeable c) => Widen (Pair a b) (Triple a b c) where
   widen o = genericWiden o get_Pair_Tail set_Pair_Tail
\end{code}

We put the two classes to use by constructing a \emph{Pair} and \emph{Triple}, insert them into a list of pairs, and map over the list projecting out the first component and printing its value.

\begin{code}
myOOTriplet = do
  p <- new $ pair    (0 :: Int)  (3.0 :: Double)
  t <- new $ triple  (0 :: Int)  (4.0 :: Double)  "Hi"

  let  pairs  :: [Pair Int Double]
       pairs  = consUb t (consUb p nilUb)

  sequence_ $ map (\p -> p # getFirst >>= print) pairs
  
  t' <- t # swapTriple
  t' # getFirst >>= print
\end{code}

\begin{verbatim}
ghci>myOOTriplet
0
0
4.0
\end{verbatim}

With parameterized classes the question of substitutability can be extended to incorporate a class' type parameters. For instance, |Pair Point Point| where |Point| is a subclass of |Object| is intuitively a subtype of |Pair Object Object|, i.e. it is safe to substitute a value of type |Pair Object Object| with a value of type |Pair Point Point| because both |getFirst| and |getSecond| are expected to return a |Point| which can safely be interpreted as an |Object|. This intuition is formalized by \emph{depth subtyping}. Unfortunately, our library is limited to a coarse form of \emph{width subtyping}. With \emph{depth subtyping} casting no longer solely dependents on the top-level type structure, but also needs access to the innards in order to change the type of method arguments and return types. This is exactly what \oohaskell's |deep'narrow| function does (see section 5.9 \cite{OOHaskell}), leaning heavily on advanced type-level programming to make such generic record traversals possible. This is where our simple OO approach begins to crack in accordance with the predictions of \oohaskell's authors. Hence, we are forced to stick with a less powerful option where the type parameters are left invariant. As a consequence it is impossible e.g. to insert a |Pair Point Point| into a list with elements of type |Pair Object Object|.

Although we cannot use casts at the type parameter position, the typing of |#| is compatible with both width and depth subtyping similar to \oohaskell where they covered this fact extensively in section 5.9 and 5.10 \cite{OOHaskell}. We have transcribed their examples without any trouble\footnote{See: \url{https://github.com/rubendg/lightoo}}.

% We start with defining a \emph{Vector} class that serves as a container for two \emph{Points}.

% \begin{code}
% data VectorClass p t = VectorClass {
%     _vectorGetP1  :: IO p
%    ,_vectorGetP2  :: IO p
%    ,_vectorPrint  :: IO ()
%    ,_vectorTail   :: Record t
% }

% vector p1 p2 =
%    (vector' `extends` object) noOverride set_Object_Tail
%    where
%    vector' tail super = do
%       p1r <- newIORef p1
%       p2r <- newIORef p2 
%       return $ \self -> VectorClass {
%           _vectorGetP1  = readIORef p1r
%          ,_vectorGetP2  = readIORef p2r
%          ,_vectorPrint  = do  self # vectorGetP1 >>= ( # pointPrint)
%                               self # vectorGetP2 >>= ( # pointPrint)
%          ,_vectorTail   = tail self
%       }
% \end{code}

% The \emph{Vector} works for any \emph{Point}. 

% \begin{code}
% testVector = do
%   p1   <- new $ printable_point 0
%   p2   <- new $ printable_point 5
%   cp1  <- new $ colored_point 10 "red"
%   cp2  <- new $ colored_point 25 "red"
%   v    <- new $ vector p1 p2
%   cv   <- new $ vector cp1 cp2
% \end{code}

% We define a separate function for calculating the norm of a vector. 

% \begin{code}
% -- Optional type
% norm :: ObjectClass (VectorClass (ObjectClass (PrintablePointClass a1)) a) -> IO Int
% norm v = do
%    p1 <- v # vectorGetP1  ; p2 <- v # vectorGetP2
%    x1 <- p1 # getX ; x2 <- p2 # getX
%    return $ abs (x1 - x2)
% \end{code}

% The vector |cv| of \emph{ColoredPoints} is a subtype of the vector |v| of \emph{PrintablePoints} by virtue of depth subtyping, i.e. |cv| has all the fields |v| has with the methods not not necessarily of the same type but related by subtyping. The subtyping rule for methods is given by $S-ARROW$ \cite{tpl}.

% $ [$\emph{S-ARROW}$] $ \inference{
%   A'_1, ..., A'_n <: A_1, ..., A_n & R <: R'
% }
% {
% A_1, ..., A_n -> IO\ R <: A'_1, ..., A'_n -> IO\ R'
% }

% In order for all occurrences of |#| in |norm| to be well-typed the co-variance (right-hand premise) of the result type of both |vectorGetP1| and |vectorGetP2| is exercised. Hence, |norm| can be applied to vectors of \emph{PrintablePoints} or any of its subtypes exemplified here:

% \begin{code}
%   -- con't
%   putStrLn "Length of v"
%   norm v >>= P.print
%   putStrLn "Length of colored cv"
%   norm cv >>= P.print
% \end{code}

% We extend \emph{Vector} to \emph{Vector1} with a method |vector1Move0| which allows us to move the origin of the vector. 

% \begin{code}
% data Vector1Class a t = Vector1Class {
%      _vector1Move0  :: a -> IO ()
%    , _vector1Tail   :: Record t
% }

% vector1 p1 p2 = 
%   (vector1' `extends` vector p1 p2) noOverride set_Vector_Tail
%   where
%   vector1' tail super =
%     return $ \self -> Vector1Class {
%        _vector1Move0 = \pa -> do
%           p1 <- self # vectorGetP1
%           x <- pa # getX
%           p1 # moveX $ x
%        ,_vector1Tail = tail self
%     }
% \end{code}

% Similar to |v| and |cv| we construct the vectors |v1| and |cv1| respectively a \emph{Vector1} with \emph{PrintablePoints} and \emph{ColoredPoints}. If we intend |v1| to be substitutable with |cv1| its methods must follow $S-ARROW$, in particular the contra-variance rule (left-hand premise) which says that argument of |vector1Move0| should be a |PrintablePoint| or any of its supertypes. We define a function that moves a vector's origin to zero. 

% \begin{code}
% move_origin_to_0 varg = do
%   zero <- new $ printable_point 0
%   varg # vector1Move0 $ zero
% \end{code}

% \begin{code}
% data Vector2Class p t = Vector2Class {
%   -- other vector methods
%   ,_vector2Set0 :: p -> IO ()
%   ,_vector2Tail :: Record t
% }

% vector2 p1 p2 =
%   (vector2' `extends` object) noOverride set_Object_Tail
%   where
%   vector2' tail super = do
%     p1r <- newIORef p1
%     p2r <- newIORef p2 
%     return $ \self -> Vector2Class {
%       ...
%       ,_vector2Set0 = writeIORef p1r 
%       ,_vector2Tail = tail self
%     }

% testVector = do
%   v2  <- new $ vector2 p1 p2
%   cv2 <- new $ vector2 cp1 cp2
% \end{code}

% \begin{code}
% align_origins va vb = do
%   pa <- va # vector2GetP1
%   vb # vector2Set0 $ pa
% \end{code}

% \begin{code}
% set_origin_to_0 varg = do
%   zero <- new $ printable_point 0
%   varg # vector2Set0 $ zero
% \end{code}

\section{Scraping the boilerplate}
\label{sec:boilerplate}

Given our decision to no use extensible records and with Haskell not being tailored towards OO programming it is only logical that there are quite a few steps involved to start implementing a new class:
\begin{enumerate} 
  \item Create a new record for representing the class' interface.
  \item Make it an instance of some |Typeable| type class.
  \item If the class is a subclass
  \begin{enumerate}
    \item Make each function available as a method by explicitly unrolling the object representation.
    \item Make it an instance of both |Narrow| and |Widen|.
  \end{enumerate}
\end{enumerate}

The first and second step are easily done manually. The third step is the most painful part where both the declaration of methods and the implementation of |Narrow| and |Widen| require nested record reads and writes which is known to be thorny issue\footnote{For an ongoing discussion see: \url{http://hackage.haskell.org/trac/ghc/wiki/ExtensibleRecords}. There are also quire some approaches that uses lenses for dealing with records \url{http://brandon.si/code/haskell-state-of-the-lens/}}.  

The goal is the scrape as much boilerplate as possible. We see three possible plans of attack to achieve this goal:
\begin{enumerate} 
  \item Create a DSL for OO programming which translates back to regular Haskell. 
  \item Use Template Haskell.
  \item Use the C pre-processor (CPP).
\end{enumerate}

A DSL will lead to the most elegant solution with minimal input required by the programmer. Furthermore, and admittedly more important it can hide all the idiosyncrasies of the encoding. Creating such a DSL does require more research and we leave it as future work. Template Haskell would be an ideal trade-off, unfortunately we cannot use it as it is GHC specific. Consequently we are left with CPP which allows us to derive some of the boilerplate but not all it. For instance, it cannot generate class methods from an interface definition. What it can do is generate the boilerplate instances and tail manipulation functions with some help from the type class system. 

We define two macros for deriving step (2) and (3 b) one for top-level classes \verb DefineClass  and the other for subclasses \verb DefineSubClass . See \ref{appendix:macros} for a description of their implementation.

\section{Discussion}
\label{sec:discussion}

\subsection{Usability}

When designing a new language one has maximal flexibility with respect to the syntax and semantics. Because we choose to embed our DSL in Haskell we inherit the limitations of the host language. The fact that implementation details reach the surface directly follows from this decision, even though the combinators and CPP macros help with hiding some of them. In particular our reliance on non-extensible records is a great source of trouble and a major factor in the abstraction leaks. Also, the peculiar combination of subtype constraints with tail-polymorphism does not result in an uniform treatment of subtyping. Uniformity is a key aspect in good language design, it contributes to the predictability of a language -- an important factor in the usability of any language. Grasping the subtle details of the library is not for the faint of heart and significantly diminishes its usability.

Besides the limitations of our OO encoding there are also some aspects of Haskell that will trouble any embedding of OO-like code in Haskell. For example, the lack of mutually recursive modules makes properly organizing OO code difficult. When applying the one interface per file scheme it often turns out that method types necessitate that modules importing each other. The lack of recursive modules in Haskell then requires all interface definitions to reside inside a single file, something which is not only cumbersome from a organizational point of view, but also breaks encapsulation at the module level.

Despite these issues we are confident that we have improved the usability of the \emph{"Mutable objects, with tail polymorphism"} approach \cite{OOHaskell}, by providing a set of useful combinators bundled inside a ready to use library. 

\subsection{Efficiency}

Contrary to \oohaskell which uses \emph{polymorphic, extensible records of closures} for their object representation we use a much simpler model: \emph{records of closures}. This decision has various implications for the efficiency of our encoding. 

Similar to \oohaskell our object representation makes no distinction between an object's data and its methods. For efficiency reasons many OO languages do make such a distinction in order to share methods across all instances of a class. However, separating the two destroys the simplicity of the approach. Because we have not attempted to implement the optimization it remains unclear if it is even possible with our encoding.

In \oohaskell method-lookup is linear in the amount of methods. Our encoding has a more efficient method-lookup which is linear in the subclass depth. Unfortunately, we do not have constant-time record extension, but due to the nested record structure record extension that is linear in the subclass depth. 

In an OO language one would typically expect that casting operationally corresponds to the identity function. \oohaskell shows in section 5.7 \cite{OOHaskell} that they support nominal subtypes by explicit nomination of the types on top of their structural record types. Using the nominal subtyping scheme they are able to implement an upcast which operationally corresponds to the identity function. They also suggest that \emph{some forms} of downcasts can be implemented, but do not provide any further details. Our implementation of casting does, unfortunately, not correspond to the identity function, but requires repeated narrowing of which the complexity is given in section \ref{subsub:upcast}. Without the use of extensible records and type-level programming there appears to be no way around this. 

\subsection{Future work}

We have focused on exploring and extending the \emph{Mutable objects, with tail polymorphism} approach. It would be interesting to see whether some of the insights gained by our exploration can be transferred to the other more primitive encodings presented in \oohaskell. Furthermore, in our exploration we have limited ourselves to Haskell with a minimal amount of language extensions, lifting this restriction may yet lead to another OO encoding. Another interesting direction to look into is to see whether there exists a proper translation of Featherweight Java \cite{Igarashi:2001:FJM:503502.503505}, a core calculus embedding the essence of Java, to our encoding.