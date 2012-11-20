\chapter{Interfacing with JavaScript}
\label{chap:jsffi}

The Utrecht Haskell Compiler can compile Haskell down to \js, however for a \js program to be of any real use it must be able to interface with the target platform. The Haskell Foreign Function Interface (FFI) addendum describes a framework for interfacing to foreign languages from within Haskell \cite{ffi}. It instantiates the framework for the C calling convention (which should be supported by all Haskell implementations), and leaves open the possibility of extending it to other calling conventions. The C calling convention is significantly different from \js's, therefore UHC has gained a new one specifically tailored for \js \cite{jcu}.

In many aspects Haskell and \js are each others opposites making it sometimes non-obvious how \js functionality should be mapped onto Haskell and \emph{vice versa}. This quickly becomes apparent when one tries to come up with meaningful type signatures for imported \js functionality. Further, Haskell data types differ quite a bit from the ones in \js leaving open the question on how to deal with conversions between the two different representations. 
When we started our research the \js FFI was work in progress, and at times our efforts mingled with that of the authors of \cite{jcu}. Our work can hence be viewed as a natural continuation of theirs with as goal making \js programming more bearable. We make the following contributions:
\begin{itemize}
  \item extend the existing infrastructure for programming with the \js FFI;
  \item augment the FFI with a new keyword for creating \js objects, and a simple way to incorporate external \js dependencies;
  \item provide a model for primitive \js types, together with type checking and marshalling functions.
\end{itemize}

The outline of the chapter is as follows: section \ref{sec:jsffi_intro} introduces the \js FFI, section \ref{sec:ty-untyped} discusses the possibilities of maintaining type-safety, section \ref{sec:marshalling} describes a simple approach for converting Haskell values from and to \js, section \ref{sec:jsidioms} shows how the \js FFI can be used to model common \js idioms.

\section{Introduction}
\label{sec:jsffi_intro}

The UHC \js FFI extends the \emph{callconv} production of the FFI grammar, found in the Haskell FFI addendum \cite{ffi}, with a new keyword |js|.

\begin{verbatim}
decl      :=  'import' callconv [safety]  impent var :: ftype
           |  'export' callconv           expent var :: ftype
callconv  :=  'ccall' | .. | 'js'
\end{verbatim}

Which calling convention is used determines how the compiler interprets the \emph{impent} and \emph{expent} strings. A formal grammar for the \emph{impent} section is given in \cite{jcu} describing a small subset of \js with only few non-\js parts used for expressing the connection between the formal arguments of a Haskell function and their position in the \js expression. Here we present a revised version of the grammar:

\begin{alltt}
impent   ::= "wrapper" || "dynamic" || jscc
expent   ::= "any string"

jscc     ::= ident '.js ' jsexpr     -- JS expression with external dependency
           || jsexpr 
jsexpr   ::= '\{\}'                    -- Haskell constructor to JS Object
           || 'new'? ptrnOrId post*   -- JS expression
post     ::= '.' ptrnOrId            -- object field
           || '[' jsexpr ']'          -- array indexing
           || '(' args ')'            -- function call
args     ::= \(\epsilon\)
           || '%*'                    -- match all arguments
           || ptrnOrId (,ptrnOrId)* 
arg      ::= '%' int                 -- index a specific argument
           || literal
ptrnOrId ::= arg 
           || ident

literal  ::= 'any character' || "any character" 
ident    ::= letter (letter || integer)*
\end{alltt}

The grammar is extended with the possibility to specify external \js dependencies, the |new| keyword for instantiating objects, and some ambiguity issues are resolved that arose from using specific combinations of \emph{match all arguments} and \emph{index a specific argument} in a single expression.

Every \emph{jsexpr} is transformed into a valid \js expression iff it is provided with a correct argument mapping (there are currently no checks in place ensuring this is the case).  Furthermore, the FFI does not check whether the imported functionality is actually present at runtime. We imagine that this could be implemented in the future by inserting runtime checks or parsing external sources statically verifying the presence of the imported functionality (not a trivial problem).

With the \js FFI in place we illustrate its use by a typical use case: displaying an alert message.

\begin{code}
foreign import js "window.alert(%1)" 
  alert :: a -> IO ()

foreign import js "'Hello World!'" 
  helloWorldStr :: a

main = alert helloWorldStr
\end{code}

Running |main| will result in the alert shown on the left in figure \ref{fig:browser_hello}. We have imported |alert| such that it can only run inside the IO monad, because we know that it has the side-effect of displaying a message box. There is, however, nothing preventing us from importing |alert| as a pure function. It is the programmer's responsibility judge whether the imported functionality is pure or not. Also, note that the first argument of |alert| can be any value. We could just as well apply it to a Haskell string:

\begin{code}
main' = return "Hello World!" >>= alert
\end{code}

The result of running |main'| is shown in the right in figure \ref{fig:browser_hello}. Because |alert| accepts any type as argument and the representation of strings in Haskell differs from \js we get garbage as output.

\begin{figure}[h]
\center
\includegraphics[scale=.4]{resources/browser_helloworld.png}
\includegraphics[scale=.4]{resources/unintented_helloworld.png}
\caption{The result of evaluating |main| on the left, and |main'| on the right.}
\label{fig:browser_hello}
\end{figure}

The problem with |alert| is fairly innocent, however it gets worse with a function like \emph{plus2}:

\begin{verbatim}
function plus2(x) {
  return x + 2;
}
\end{verbatim}

In general \js functions can take any number of arguments with any number of types and in all but few cases result in a \emph{TypeError} \cite{jstypesys}. Conform \js we leave the first argument of \emph{plus2} polymorph:

\begin{code}
foreign import js "plus2(%1)"
  plus2 :: a -> IO Int
\end{code}

However, again |plus2| does not prevent us from passing in e.g. a boolean instead of a number. When we do \js will coerce the boolean to a number, perform the addition, and return the result with the coercion going unnoticed. While in some cases this is intended behavior, there are many more cases where these coercions are plain programming errors. The silent coercions make it difficult to localize bugs and it gets worse when the size of the code base increases. The fact that tools such as Google Closure \cite{bolin2010closure} are created, that help with type checking, proves that this is in fact a real problem. In the next section we will see how we can give functions like |alert| and |plus2| a better type signature.

\section{Typing the Untyped}
\label{sec:ty-untyped}

In terms of type systems Haskell and \js are each others opposites. Haskell has a strong static type system, i.e. the type rules are checked at compile-time and type inconsistencies are reported to the user. A program that passes the type checker is sound with respect to the type-rules (i.e. if a program is well-typed it cannot cause type errors), hence type consistency checks can be omitted which leads to faster object code. On the other hand in \js all type checking happens at runtime, types are never specified, variables derive their types from the value they point to at runtime, and type inconsistencies are resolved by implicit type conversions.

The \js FFI opens up the beautifully consistent Haskell world to the unsafe \js world. We would like to move away from importing \js functions with all arguments left polymorph to a situation where we can be more precise about a function's type. This should lead to more idiomatic Haskell and thus allow us to benefit from static guarantees made by the Haskell type system. Before we can annotate functions with more precise types we first need a Haskell model of the \js types. We imagine that these annotations may in the future be used to automatically insert runtime type checks, but for now we will resort to manual type checking. We will discuss the proposed \js type model, type checking, and how to deal with union types. 

\subsection{A model for \js types}
\label{subsec:modeljstypes}

The \js language definition \cite{ecmascript} describes several primitive types: undefined, null, boolean, number, and string; as well as several kinds of objects (plain objects, wrapper objects, function objects, array objects, regex objects). The primitive types bool, number, and string each have a corresponding wrapper object with an bi-directional conversion between each pair. Some operations (like \emph{+}) only work for particular primitive types. When these functions receive a value of an other type than the expected type they automatically coerce the value to the expected type.

Of the primitive types we only model \emph{undefined} and \emph{null}. For the others we only model their wrapper object letting it range over values of the primitive type as well as their wrapper object. We piggyback on the coercion semantics of \js, which ensures that we can always use a primitive type as if it were a wrapper object. Furthermore, we have |JSAny| range over all \js types. Figure \ref{fig:js-typehierarchy} shows how the types are related to each other.

\begin{figure}[h]
\begin{tikzpicture}[level distance=10mm,level/.style={sibling distance=30mm/#1}, font={\small}]
\node {JSAny}
  child { 
    node {JSUndefined}
  }
  child {
    node {JSNull}
  }
  child {
     node {JSObject} {
          child {
            node {JSBool}
          }
          child {
            node {JSString}
          }
          child {
            node {JSFunction} 
          }
          child {
            node {JSRegex}
          }
          child {
            node {JSArray}
          }
          child {
            node {$\dots$}
          }
      }
  };
\end{tikzpicture}
\caption{A model of the \js types.}
\label{fig:js-typehierarchy}
\end{figure}

To translate the model to Haskell we make fruitful use of opaque types for \emph{undefined} and \emph{null}, and of phantom types to model a hierarchy of types \cite{wxhaskell,Fluet:2006:PTS:1180085.1180088}. 

\begin{code}
data JSAny a

data CJSUndefined
type JSUndefined = JSAny CJSUndefined

data CJSNull
type JSNull = JSAny CJSNull

data CJSObject  a
type JSObject_  a  = JSAny (CJSObject a)
type JSObject      = JSObject_ ()

data CJSBool
type JSBool = JSObject CJSBool

type JSString = JSObject PackedString

data CJSFunction a
type JSFunction_ a = JSObject (CJSFunction a)

data CJSRegex
type JSRegex = JSObject CJSRegex

type JSArray v = JSObject (BoxArray v)
\end{code}

Both |PackedString| and |BoxArray| are UHC specific types used internally for representing respectively plain strings and arrays. |JSObject_| and |JSFunction_| take an additional type parameter which can later be refined to either extend the hierarchy, or make a function type explicit. Note that we do not have a type for \js numbers because all non-aggregate types like |Int|, |Float| and |Double| are shared with the \js. |Integer| is the only exception, because \js has no native support for arbitrary-precision integers an |Integer| value is wrapped by the RTS inside a BigInt object. 

With the model for \js types we can now give both |alert| and |helloWorldStr| a more precise type:

\begin{code}
foreign import js "'Hello World!'" 
  helloWorldStr :: JSString

foreign import js "window.alert(%1)" 
  alert :: JSString -> IO ()
\end{code}

The DOM defines many interfaces for communicating with the web browser. Each interface corresponds to an object in \js, and we can now easily model these interface types by extending |JSObject_|. As example we use the \emph{Node} interface:

\begin{code}
data CNode  a
type Node_  a  = JSObject (CNode a)
type Node      = Node_ ()
\end{code}

The |nodeType| function is defined for any \emph{Node}. Using the new type we just introduced we can easily express this by leaving the extension of the node polymorph:

\begin{code}
foreign import js "%1.nodeType"
  nodeType :: Node_ a -> IO JSString
\end{code}

Now |nodeType| can be applied to all objects that are \emph{at least} of type \emph{Node}. This is works using ordinary type unification and enables a form of subtype polymorphism.

Despite all the effort we may spend on typing \js functionality there are plenty opportunities to undo any assumptions made about the types at runtime, e.g. nothing prevents us from changing the definition of \emph{window.alert} to:

\begin{verbatim}
window.alert = undefined;
\end{verbatim}

There is not much we can do about this and similar to the Closure compiler we are forced to make some assumptions about the runtime behavior to ensure consistency:
\begin{itemize} 
  \item all imported JavaScript functions and object properties do not change types at runtime;
  \item prototype chains do not change at runtime.
\end{itemize}

\subsection{Type checking}

% The problem

As soon as data crosses the language border performing runtime type checks to preserve type safety becomes inevitable \cite{Abadi89dynamictyping}. Take for example the \emph{createElement} function:

\begin{code}
foreign import js "document.createElement(%*)"
  createElement :: JSString -> IO (Element ())
\end{code}

Dependent on which string we pass to |createElement| we get back a different subtype of \emph{Element}. For this function to be useful we must be able to determine the actual return type by discriminating on the value's type at runtime. Once we have verified the value to be of a particular, more specific, type we may interpret it as such using a cast.

% The dynamics solution

One approach to implement runtime type checking of \js values would be to reuse the |Data.Dynamic| machinary which works by packing together a value with its type representation |TypeRep| in a data type called |Dynamic|. A |Dynamic| supports type safe projection of its contained value by comparing its |TypeRep| against an expected |TypeRep|. The difficulty lies in constructing a (|TypeRep|) for a given \js value. There are two design alternatives: delegate the task to the runtime system or perform the mapping inside Haskell. The first approach leaks knowledge of data type compilation into the RTS thereby complicating it. The second, does not, but requires a larger family of type checking functions from the RTS to build up the |TypeRep| in Haskell.

Both design alternatives deserve further exploration, but we leave this as future work and for now resort to a much simpler approach. We simply extend the RTS with a family of type checking functions that given a value return either true or false dependent on whether the value's type matches the expected type. 

\begin{code}
isNull, isUndefined, isBool, isString, 
isChar, isInt, isDouble, isFloat, isNumber, isObject, isFunction :: a -> Bool
\end{code}

The type checking functions are implemented in terms of \emph{typeof}, which given a value returns a string describing its type. Here are the implementations of |isFunction| and |isBool|:

\begin{multicols}{2}
  \begin{verbatim}
  primIsFunction = function(a) {
    return PrimMkBool(
      typeof a === "function"
    );
  }

  primIsBool = function(a) {
    return PrimMkBool(
         typeof a === "boolean" 
      || _primIsA(a, Boolean)
    );
  }
  \end{verbatim}
  \vfill
  \columnbreak
  \begin{code}
    foreign import js "primIsBool(%*)"
      isBool :: a -> Bool

    foreign import js "primIsFunction(%*)"
      isFunction :: a -> Bool
  \end{code}
\end{multicols}

The implementation reflects our decision to treat primitive and wrapper types as one and the same. We use \emph{PrimMkBool} to directly create values of the Haskell type |Bool|. It embodies knowledge about how the compiler represents data types; like many other functions defined in the RTS. The \emph{primIsA} function checks whether an object is exactly of some type by inspecting its constructor value.

\begin{verbatim}
_primIsA = function(a, b) {
  if(typeof a === "object" && a !== null && typeof b === "function") {
    return a.constructor == b;
  }
  return false;
}
\end{verbatim}

To transitively test whether an object is of particular type we implement another function in terms of \emph{instanceof}:

\begin{verbatim}
primIsInstanceOf = function(a, b) {
  if(typeof a === "object" && typeof b === "function") {
    return PrimMkBool(a instanceof b);
  }
  return PrimMkBool(false);
}
\end{verbatim}

\pagebreak

Using \emph{primIsInstanceOf} we can implement the \emph{cast} function we mentioned earlier on which guards the type conversion with a type check.

\begin{multicols}{2}
\begin{code}
class JSCtor a where
  jsCtor :: a -> b

cast :: JSCtor b => a -> Maybe b
cast a :: Maybe b = 
  if instanceOf a (jsCtor (undefined :: b))
    then Just (unsafeCoerce a)
    else Nothing
\end{code}
\vfill
\columnbreak
\begin{code}
foreign import js "primInstanceOf(%*)"
  instanceOf :: a -> b -> Bool

instance JSCtor (HTMLDivElement ()) where
  jsObjectConstructor _ = htmlDivelementType

foreign import js "HTMLDivElement"
  htmlDivelementType :: HTMLDivElement ()
\end{code}
\end{multicols}

With |cast| we can define a function |createDivElement| for creating \emph{HTMLDivElement}s in terms of |createElement|. 

\begin{code}
createDivElement = do
  e <- createElement divString
  case cast e :: Maybe (HTMLDivElement ()) of
    Just x   -> x
    Nothing  -> error "Something went wrong"
\end{code}

\subsection{Representing union types}
\label{chap:ffi:uniontypes}

Many \js functions take/return an union of types, best illustrated by an example:

\begin{verbatim}
function foo(b) {
  if(b) {
    return "an";    (1)
  } else {
    return false;   (2)
  }
}
\end{verbatim}

The if statement works for any type by coercing its argument to a value of type \emph{boolean}. Dependent on which branch is taken the result of \emph{foo} will either be of type \emph{string} (1) or \emph{boolean} (2). Keeping up the spirit of providing type annotation \emph{foo}'s type can be best described by: 

\begin{verbatim}
foo :: a -> JSBool + JSString
\end{verbatim}

where it takes any type to an union containing either a |JSBool| or |JSString|. The question is: how do we effectively represent a union of types in Haskell? In the following section we will discuss three different approaches: hand-made universes, dynamics, and extensible sums. 

\begin{code}
foreign import js "foo"
  _foo :: a -> ?
\end{code}

\subsubsection{Hand-made universes}

The standard Haskell library comes with the |Either a b| data type with which we can represent a binary union.

\begin{code}
data Either a b = Left a | Right b
\end{code}

We can use |Either| to wrap |_foo|, scrutinize the return value using a type check, and associate it with either a |Left| or |Right| tag. The use of |unsafeCoerce| is unavoidable because we gain knowledge about the types \emph{at runtime} that cannot be legitimized at compile-time. 

\begin{code}
foo :: a -> Either JSString JSBool
foo a =
   let ret r  | isString  r  = Left   (unsafeCoerce r)
              | isBool    r  = Right  (unsafeCoerce r)
   in ret (_foo a)

foreign import js "foo(%1)" 
   _foo :: a -> b
\end{code}

Even though the above encoding works fine it does not generalize nicely to n-ary union types. Especially the manual injection and projection of nested |Either| data types quickly becomes cumbersome. 

\subsubsection{Dynamics}

Using the |Data.Dynamic| library we can hide values of different types in a single value of type |Dynamic|. To make the running example a bit more interesting we cook up a new function with a slightly more complicated type (implementation is not important):

\begin{code}
bar :: JSNull + Int -> JSBool + JSString
\end{code}

The argument and result type of |bar| collapse into a |Dynamic| type.

\begin{code}
bar :: Dynamic -> Dynamic 
bar d =
    let jsVal =
          case fromDynamic d :: Maybe JSNull of
            Just v   -> unsafeCoerce v
            Nothing  -> case  fromDynamic d :: Maybe Int of
                              Just v    -> unsafeCoerce v
                              Nothing   -> error "impossible"

        ret r  |  isString  r  = toDyn (unsafeCoerce r :: JSString)
               |  isBool    r  = toDyn (unsafeCoerce r :: JSBool)
    in ret (_bar jsVal)

foreign import js "bar(%1)" 
   _bar :: a -> b
\end{code}

To invoke |bar| we pre- and suffix it with dynamic unwrapping and wrapping calls. 

\begin{code}
(fromDynamic . bar . toDyn) _null :: Maybe JSString
\end{code}

Unfortunately, the projection (|fromDynamic|) and injection (|toDyn|) functions only work on monomorphic types that are instances of |Typeable|. Because |_bar| does not care about the type of its argument we can freely use |unsafeCoerce| as an escape hatch making the type system forget that it actually knows the type of |v|. In the result position we can use the same trick to let the type system learn about the new types.

Using dynamics is significantly simpler when dealing with n-ary union types compared to hand-made universes. However, there are several shortcomings:
\begin{itemize}
  \item limited to injecting and projecting monomorphic types;
  \item the |Dynamic| type has no descriptive value.
\end{itemize} 

Unfortunately, the fact that dynamics are limited to monomorphic types does not align well with our encoding of objects. Suppose we have a function with type:

\begin{code}
getNodeType :: Node a + JSNull -> IO Int
\end{code}

A value of type |Node a| cannot be injected into a |Dynamic|. What we can do is temporarily make the type monomorph by flagging the type parameter position with a special data type.

\begin{code}
data TyVar
   deriving Typeable
\end{code}

Using a pair of injection and projection functions we can turn a polymorphic into a monomorpic type and the other way around.

\begin{code}
prjNode :: Dynamic -> exists a. Maybe (Node a)
prjNode = unsafeCoerce . (fromDynamic :: Dynamic -> Maybe (Node TyVar))

injNode :: Node a -> Dynamic
injNode = toDyn . (unsafeCoerce :: Node a -> Node TyVar)
\end{code}

The |prjNode| function returns a value of type |Maybe (Node a)| where the type variable |a| is existentially quantified over preventing the caller from instantiating it to anything other than a polymorphic type variable. 
Unfortunately, the function pair is type specific, and we would like to abstract from the specifics using a type class:

\begin{code}
class Iso f where
  inj :: f a -> Dynamic
  prj :: Dynamic -> exists a. Maybe (f a)
\end{code}

However, it turns out that this does not help much as the nested types cannot be directly used to create instances of |Iso| for. First, they need to be wrapped inside a newtype such that they can be partially applied.

\begin{code}
newtype One    a x      = One    { unOne    ::  a x          }
newtype Two    a b x    = Two    { unTwo    ::  a (b x)      }
newtype Three  a b c x  = Three  { unThree  ::  a (b (c x))  }  
...
\end{code}

The programmer still needs to manually inject and project its type in and out of a member of the newtype family, nothing is gained in terms of usability. 

\begin{code}
instance (Typeable1 a, Typeable1 b) => Iso (Two a b) where
   inj    = toDyn . (unsafeCoerce :: a (b x) -> a (b TyVar)) . unTwo
   prj d  = 
    case fromDynamic d :: Maybe (a (b TyVar)) of
      Nothing  -> Nothing
      Just x   -> Just (Two $ unsafeCoerce x)
\end{code}

\subsubsection{Extensible unions}

Using type classes binary unions can be generalized to n-ary union types with automatic injection and projection functions \cite{modularintrp,alacarte}. The trick is to rely on a right-associative nesting of types, and let type class instances generically traverse the type structure to either inject or project a type. First, we define a binary union:

\begin{code}
data a :|: b  = L a | R b
infixr 5 :|:
\end{code}

Using nested constructor applications of this data type we can write, e.g., a ternary union:

\begin{code}
R (R True) :: Int :|: String :|: Bool
\end{code}

The injection and projection functions of the |SubType| type class automatically find value level injections and projections for values of type |:||:|. 

\begin{code}
class SubType sub sup where
   inj  :: sub -> sup          -- injection
   prj  :: sup -> Maybe sub    -- projection
\end{code}

The implementation of |inj| and |prj| is covered by the following instances:

\begin{code}
instance SubType a a where
  inj = id
  prj = Just

instance SubType a (a :|: b) where
  inj        = L
  prj (L x)  = Just x
  prj _      = Nothing

instance (SubType a c) => SubType a (b :|: c) where
  inj        = R . inj
  prj (R x)  = prj x
  prj _      = Nothing
\end{code}

The first instance states that |SubType| is reflexive. The second instance states for injection that if we have a value of type |a| we can inject it into |a :||: b|, and for projection that provided with a value of type |a :||: b| we can project out its value if it matches |L|. The third instance asserts for injection that provided we can inject a value of type |a| into |c| we can also inject |a| into a larger type |b :||: c| by composing the first injection with an additional |R|, and for projection that provided we can project |a| out of |c| we can also project out |a| from a larger type |b :||: c| if its value matches |R|.

Using extensible unions we may rewrite |bar| such that its type becomes much more informative compared to the dynamics approach.

\begin{code}
bar :: JSNull :|: Int -> JSString :|: JSBool
bar a = 
   let jsVal = 
         case prj a :: Maybe JSNull of
          Just v   -> unsafeCoerce v
          Nothing  -> 
            case prj a :: Maybe Int of
              Just v   -> unsafeCoerce v
              Nothing  -> error "impossible"

       ret r  |  isString  r = inj (unsafeCoerce r :: JSString)
              |  isBool    r = inj (unsafeCoerce r :: JSBool)
   in ret (_bar jsVal)
\end{code}

Similar to the dynamics approach a call to |bar| should be wrapped with projection and injection functions:

\begin{code}
(prj . bar . inj) _null :: Maybe JSString
\end{code}

It gets interesting if union types also contain polymorphic types:

|getNodeType :: Node a :||: JSNull -> IO Int|

Suppose we want to call |getNodeType| with a value of a type that unifies with |Node a|. Injecting this value into the argument type is going to fail, unless we provide a type annotation which instantiates the type variable to a concrete type matching the given type. For example, we could apply |getNodeType| to a HTMLElement as follows:

|getNodeType (inj (undefined :: HTMLElement ()) :: HTMLElement () :||: JSNull)|

We can even inject a polymorphic value of type |Node a| given that we provide type annotations, and use scoped type variables to ensure that identically named type variables are considered the same. The same rules hold for projection.

|getNodeType (inj (undefined :: Node a) :: Node a :||: JSNull)|

Of the three alternatives this one is the clear winner. It requires less boilerplate compared to the first approach, and provides more informative types than the second. Also, it is more flexible than dynamics as it naturally allows polymorphic types inside a union type as long as they do not overlap.  

\section{Marshalling}
\label{sec:marshalling}

Aggregate Haskell types do not map directly onto \js types. The difference in data representation calls for conversions functions between Haskell data, and their \js equivalent. There are cases where such a mapping is obvious, e.g. a |Bool| simply maps to the \js boolean type. However, there are also cases where such a mapping is less obvious, e.g. in the case of |Maybe a|. To express the mapping between Haskell values, \js values, and \emph{vice versa} we imagine two mapping functions |haskToJS| and |jsToHask| for some of the standard Haskell types:

\begin{multicols}{2}
\begin{code}
haskToJS :: Haskell -> JS
haskToJS ()         = undefined
haskToJS True       = true
haskToJS False      = false
haskToJS ""         = "" 
haskToJS Nothing    = null
...
\end{code}
\vfill
\columnbreak
\begin{code}
jsToHask :: JS -> Haskell
jsToHask undefined  = ()
jsToHask true       = True
jsToHask false      = False
jsToHask ""         = "" 
jsToHask null       = Nothing
...
\end{code}
\end{multicols}

Besides the obvious mappings there are also a few interesting ones. In particular, unit (|()|) which is most often used as a dummy value for expressing that a function returns no meaningful result, hence it maps naturally to the \js \emph{undefined} value returned when a function has no result. Also, |Nothing| which we could have mapped to undefined, but did not because there is a subtle difference between undefined and null. An undefined value is most often used in cases where something really is undefined, e.g. when accessing a non-existent object property, whereas null can be used by the programmer to explicitly state that something is not defined very much like |Nothing|. 

Because both mappings are only defined for of few standard Haskell types, and they both range over a set of types in their argument as well as in their result type, we model them using a type class:

\begin{multicols}{2}
\begin{code}
class ToJS a b where
  toJS :: a -> b
\end{code}
\vfill
\columnbreak
\begin{code}
class FromJS a b where
  fromJS :: a -> Maybe b
\end{code}
\end{multicols}

Although for the standard types there exist a bidirectional mapping this may not always be to case for very instance of either class, which is why we use separate type classes. Also, because converting from a \js value to a Haskell value may go wrong the |fromJS| function is partial.

For booleans the implementation is straight-forward.

\begin{multicols}{2}
\begin{code}
instance ToJS Bool JSBool where
  toJS True   = jsTrue
  toJS False  = jsFalse

foreign import js "true"   
  jsTrue  :: JSBool
foreign import js "false"  
  jsFalse :: JSBool
\end{code}
\vfill
\columnbreak
\begin{code}
instance FromJS JSBool Bool where
  fromJS v = 
    if isBool v 
      then if jsEq jsTrue v 
          then Just True
          else Just False
      else Nothing
\end{code}
\end{multicols}

The |jsEq| function is a wrapper around the \js strict equality operator. Every time a |JSBool| is converted to a |Bool| some type checking is performed. For booleans this conversion is relatively inexpensive, but for list-like structures this can become much more expensive. Hence, in the future compiler support for other strings representations (using overloaded strings) is unavoidable in order to gain performance. For now the conversion of Haskell to \js strings takes linear time in the length of the string. 

Until now we have only considered simple Haskell types, but when interfacing to \js libraries it is very useful to be able to convert a Haskell record to plain \js object. The authors of \cite{jcu} posited some design alternatives, and decided on a solution where with the help of the RTS a record can be converted to a \js object by forcing its components WHNF. The functionality is exposed through the FFI using the "\{\}" notation.

\begin{code}
data JSBook_
type JSBook = JSObject_ JSBook_

foreign import js "{}"
  mkJSBook :: Book -> IO JSBook

data Book = Book {author :: JSString , title :: JSString, pages :: Int }

book = mkJSBook (Book { author = toJS "me", title = toJS "story", pages = 123})
\end{code}

The result of applying |mkJSBook| to a |Book| value is a simple \js object:

\begin{verbatim}
{author : "me", title : "story", pages : 123}
\end{verbatim}

The opposite conversion is left for the programmer to implement.

\section{JavaScript Idioms}
\label{sec:jsidioms}

In this section we will explore how the \js FFI can be used to deal with common \js idioms.

\subsection{Instantiating objects}

In \js objects are instantiated using the \emph{new} keyword. Its absence in the former incarnation of the \js FFI has led us to consider lifting it into a primitive function.

\begin{code}
foreign import js "primNew('B', %1, %2)" 
  newB :: a -> b -> IO c
\end{code}

\begin{verbatim}
function primNew(obj) {
  var args = Array.prototype.slice.call(arguments); 
  args.shift(); 
  var l = arg.length;
  switch(l) {
    case 0: return new obj;
    case 1: return new obj(args[0]);
    case 2: return new obj(args[0], args[1]);
    case 3: return new obj(args[0], args[1], args[2]);
  }
  throw new Error('Too many arguments, not supported.');
}
\end{verbatim}

Unfortunately, the lifted version does not scale to an arbitrary number of constructor arguments. There are other solutions such as implementing it as a method on the Function object, suggested in \cite{crockford2008javascript}. However, since its part of the language we simply choose to expand the \js FFI. Its usage is no different from that in \js:

\begin{code}
foreign import js "new String(%*)" 
  newString :: JSString -> IO JSString
\end{code}

\subsection{Higher-order call}

\js functions can be passed as arguments to other functions. If we want to pass a Haskell function to a higher-order \js function we must first wrap it as a \js function. 

\begin{code}
foreign import js "twice(%*)"
  _twice :: JSFunction_ (IO ()) -> IO ()

foreign import js "wrapper"
  _twice_hof :: IO () -> JSFunction_ (IO ())

twice :: IO () -> IO ()
twice = _twice . _twice_hof
\end{code}

\begin{verbatim}
function twice(f) {
  f();
  f();
}
\end{verbatim}

The |JSFunction_| type can be seen as a small box wrapped around the original Haskell function, which can be used by regular \js code as if it were a normal function. However, internally the Haskell calling convention is maintained, and when it returns back into the \js world its return value is evaluated to WHNF. 

It is not uncommon for \js functions to return a function, the |createCounter| is an example of such a function. When invoked it returns a new function that at every call increments a variable and returns it.

\begin{verbatim}
function createCounter() {
  var i = 0;
  return function() {
    return i++;
  }
}
\end{verbatim}

We can import \emph{createCounter} just like any other function, except that when we invoke it we use the dual of "wrapper", called "dynamic", which takes a \js function and returns a Haskell function wrapping the \js function.

\begin{code}
foreign import js "createCounter()"
   createCounter :: IO (JSFunction (IO Int))

foreign import js "dynamic"
   mkCountFunc :: JSFunction (IO Int) -> IO Int
\end{code}


We can now use |createCounter| to create a counter function, which we use to print incrementing numbers to the screen.

\begin{code}
main = do
  counter <- createCounter
  let count = mkCountFunc counter
  mapM_ (\m -> m >>= print) [count,count,count]
\end{code}


\subsection{Exporting Haskell functions}

When integrating with existing code it is useful to be able to call Haskell functionality from \js. The FFI \emph{export} directive provides such functionality by exporting a Haskell function under a stable name. 

\begin{code}
minus :: Int -> Int -> Int
minus x y = x - y 

foreign export js "minus"
  minus :: Int -> Int -> Int
\end{code}

The compiler generates a wrapper around the |minus| function, and it can be called using the name given to it in the export declaration prefixed with the module name.

\subsection{Behavior of this}

In \js the \emph{this} keyword has a rather peculiar semantics different from many other OO like languages. It is different in that it is dynamic, i.e. it adapts to whatever object it is called through. This, in combination with higher-order functions, can cause problems with what \emph{this} is expected to refer to inside the body of a wrapped function. 

For example, in jQuery\footnote{\url{http://www.jquery.com}}, when an event handler is triggered jQuery executes the handler with \emph{this} set to the event source. The problem is that we do not have access to \emph{this}, and because our Haskell callback is wrapped by the compiler such that jQuery can execute it as a normal \js function the event source is also lost. There seems to be no general solution to this problem. This solution proposed in \cite{jcu} is to reify \emph{this} as an additional parameter to the callback function using a helper function:

\begin{verbatim}
function wrappedThis(f) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    args.unshift(this);
    return f.apply(this, args);
  } 
}
\end{verbatim}

We illustrate how this is done for registering click events:

\begin{code}
foreign import js "%1.click(%2)"
  _registerClick :: JQuery -> JSFunction (EventSource -> IO ()) -> IO ()

foreign import js "wrapper"
  mkCb :: (EventSource -> IO ()) -> IO (JSFunction (EventSource -> IO ()))

foreign import js "wrappedThis(%1)"
  mkWrappedThis :: JSFunction (a -> IO ()) -> IO (JSFunction (a -> IO ()))

registerClick :: JQuery -> (EventSource -> IO ()) -> IO ()
registerClick jq f = mkCb f >>= mkWrappedThis >>= _registerClick jq
\end{code}

\subsection{Optional arguments}

In \js all function arguments are optional by default. When an argument is not provided it simply defaults to \emph{undefined}.

\begin{verbatim}
function foo(x, y) {
  if(!y) {
    y = 0; // default value
  }
  return x + y;
}
foo(3);
\end{verbatim} 

The closest correspondence in Haskell to optional arguments is an argument of type |Maybe|, or when there are many options a record with defaults for every selector. The easiest way to deal with \js functions with optional arguments is to import several versions of the same function:

\begin{code}
foreign import js "foo(%1)"
  foo1 :: Int -> Int

foreign import js "foo(%1, %2)"
  foo2 :: Int -> Int -> Int
\end{code}

Although this is an easy solution it is far from pretty, and quickly explodes when the number of optional arguments increases. A better option would be to import the function with all of its arguments, and write a wrapper function that uses a |Maybe| for all optional arguments. 

\begin{code}
foreign import js "foo(%*)"
  _foo :: Int -> a -> Int

foo :: Int -> Maybe Int -> Int
foo a Nothing  = _foo a jsUndefined
foo a (Just x) = _foo a x

\end{code}

\subsection{Global state}

\js is at its core a language with mutable state, i.e. at each statement the value pointed to by a variable may change. The ability to import global state is a practical necessity. In Haskell to goto model for mutable state are IORefs. However, they are meant for modeling mutable references to immutable Haskell values, not references to mutable \js values. We want changes to the global state to immediately reflect in our reads, i.e. in effect two consecutive reads of the same piece of global state may yield entirely different values. 

We create an interface, very similar to that of IORef, for importing mutable \js state. The differences lie in the creation of a mutable reference, and the ability to distinguish between read and read-write references.

\begin{code}
-- Wraps a getter and setter 
data Lens a = Lens (IO a) (a -> IO ())

-- Use a phantom type as flag for read or read and write capabilities
newtype JSRef t a = JSRef (Lens a)

data Read
data ReadWrite

newJSRef          :: IO a -> (a -> IO ()) -> JSRef ReadWrite a
newReadOnlyJSRef  :: IO a -> JSRef Read a  
readJSRef         :: JSRef t a -> IO a
writeJSRef        :: JSRef ReadWrite a -> a -> IO ()
\end{code}

As a simple example on how |JSRef|s can be used we import a piece of global \js state (x), and an accompanying mutator (mutX).

\begin{multicols}{2}
\begin{verbatim}
x = 0;
function mutX() {
   x += 10;
}
\end{verbatim}
\vfill
\columnbreak
\begin{code}
foreign import js "x"
  readVarX :: IO Int

foreign import js "mutX()"
   mutX :: IO ()
\end{code}
\end{multicols}

We disallow writes to \emph{x}, and hence create a read-only |JSRef|. The following fragment illustrates how
changes made by the |mutX|, outside the grip of Haskell, are reflected in the value read through |readJSRef|:

\begin{code}
globSt =
  refX <- newReadOnlyJSRef readVarX
  x <- readJSRef refX
  putStr (show x)
  mutX 
  x <- readJSRef refX
  putStr (show x)
\end{code}

The |JSRef| interface is not only useful for importing global state, but also for modeling a more Haskell like interface to object properties. Furthermore, using |JSRef| instead of |IORef| for partially applying event handlers with global state solves the problem of having stale values, where the authors of \cite{jcu} struggled with. 

\subsection{Variadic functions}

\js functions can take an arbitrary number of arguments. A typical example of such a function is the string concatenation function \emph{concat} (a pure function). Similar to how we dealt with optional arguments we can import \emph{concat} by importing different versions. 

\begin{code}
foreign import js "%1.concat(%*)"
  concat1 :: JSString -> JSString -> JSString

foreign import js "%1.concat(%*)"
  concat2 :: JSString -> JSString -> JSString -> JSString
\end{code}

However, this is a poor choice as it does not truly uphold the semantics of \emph{concat}. A better option would be to use the \js \emph{apply} function. Where \emph{apply} is defined as:

\begin{verbatim}
fun.apply(thisArg[, argsArray])
\end{verbatim}

Its first argument is where \emph{this} is going to point to when \emph{fun} is called, and the second argument is an array with function arguments. Using \verb apply  we can rewrite |concat| such that it works for an arbitrary number of arguments.

\begin{code}
foreign import js "%1.concat.apply(%*)"
  _concat :: JSString -> JSString -> JSArray JSString -> JSString

concat :: JSString -> JSArray JSString -> JSString
concat x xs = _concat x x xs
\end{code}

While we think this to be an acceptable encoding it still does not truly encode variadic functions. It has been shown that variadic functions can be simulated in Haskell using type classes\cite{typesafefuncforio, Asai:2009:TDC:1743339.1743382}, but they are not commonly used and we instead stick with the more lightweight approach. 

\section{Linking \js libraries}

Web applications are constructed using a multitude of technologies. They use HTML in combination with CSS to convey rendering information to the browser, and use \js for adding interactivity to an otherwise static rendering of the HTML tree. The technology triad constitutes the corner stone of every web application, which is served to the end-user by means of a HTML document that links all necessary \js and CSS resources together.

In the current UHC pipeline, shown in figure \ref{fig:uhcpipeline}, a Haskell program is compiled down to \js, and linked into a single HTML file together with all its module dependencies, and the RTS. Without optimizations UHC uses a HTML script tag to link each dependency into the HTML file. With whole program linking turned on it links all dependencies into a single file. 

The compilation pipeline delivers a very basic web application. There are, however, many possible configurations to packacke a web application. It need not even be a single binary, but may be spread over several independent units that may be loaded using variety of linking strategies. Furthermore, how a web application is assembled and deployed depends very much on the type of web application. Of all these aspects UHC currently does not address:

\begin{itemize}
  \item external \js dependencies;
  \item inclusion of CSS files;
  \item inclusion of HTML markup;
  \item post-processing.
\end{itemize}

Although it is possible to let UHC deal with all these issues we deem it not wise to do so. The purpose of UHC is to compile Haskell to \js, and the different concerns of assembling, post-processing, and application distribution should be the task of some other software product. In the future UHC should no longer generate a HTML file itself, but produce a manifest containing a list of dependencies with which other tools can create a web application.

Contrary to the assemble process, the specification of external \js dependencies is a something UHC should allow for. \js FFI declarations import functionality that may depend on the presence of some external \js library. To make the compiler aware of external dependencies there should some interface for conveying this information to the compiler. The dependencies could be specified in a special dependency file, but this requires a new file format, and adds to the semantic distance between the \js FFI declarations and the supporting \js code. A better approach would be to reuse the existing infrastructure and specify the dependencies at either module or function level.

\begin{code}
-- Module level
{-# INCLUDE "jquery.js" #-}
module JQuery where

-- Function level
foreign import js "jquery.js %1.append(%*)"
  append :: a → b → IO ()
\end{code}

Specifying dependencies at the module level has as advantage that is it not necessary to repeat it for every function. However, this ease of use comes at the cost of loosing granularity in the linking process. Also, for flexibility reasons, the task of resolving the filename to an absolute location should be a responsibility of the compiler (search paths should be supplied as a compiler option).

We have implemented a proof of concept for the function level interface, because GHC has deprecated the language pragma, and the function level interface provides more granularity. We found that there was no infrastructure present for letting \emph{Haskell Interface} (HI) files carry external dependencies of any sorts. Hence, in the current implementation only dependencies specified in the \emph{Main} module will be considered during the linking process. In the future this should, evidently, be extended to all modules. 

% \section{Automated \js FFI generation}

% The usefulness of using Haskell to program web applications will greately increase if there would exist interfaces to HTML5 and popular \js libraries. Given the extend of these APIs the manual creation and maintenance of such interfaces would require a significant effort, hence we look into the possibilities of automatically generating them. 

% The interface exposed to \js by the web browser are defined in a language neutral format called the \emph{Web Interface Definition Language} (WebIDL). Unfortunately, the WebIDL definition does not come with an official parser. There used to be automated tooling from the \emph{York Haskell Compiler} to read WebIDL and generate Haskell definitions, but this project seems to be abandoned. 

% Apart from interfacing with the web standards the possibility of interfacing to existing JavaScript library is equally important. We see two potential options: interface extraction through type inference, or through type annotations. Type inference is, however, shown to be non trivial \cite{Vardoulakis:2010:CFA2}. DoctorJS \footnote{\url{http://doctorjs.org/}} provides a partial implementation, but although it seems promising its still far from finished. An alternative provided by some libraries is the use of explicit type annotations (JSDoc) for functions, constructors, and variable definitions. The disadvantage of this approach is that type annotations, located in comment blocks, are not checked for consistency by the language. Also, JSDoc is still work in progress and the tool support for extracting JSDoc annotations (not only for generation documentation) from \js source code is still in its developing stages. 

% Concluding, given the current state of the art it seems impossible to write an interface generator without spending significant effort on it. However, the development of such a tool will in the long run be a determining factor in the adoption of the UHC \js backend. For now manually crafted interfaces will have to do.


\section{Related work}

The omnipresence of \js makes it an attractive target language. There have already been many attempts at compiling languages to \js\footnote{A listing of languages that compile to \js: \url{https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS}}, of which the Google Web Toolkit (GWT) \cite{hanson2007gwt} (Java to \js) has undoubtedly seen most traction among the commercial programming community. Unlike GWT the compilation of Haskell to \js is still very much in its developing stages. Especially the FFI to \js is still under developed. In the following sections we will discuss some of the more prolific attempts at compiling Haskell to \js, and in particular how their FFI implementation compares to UHC's. 

\subsection{York Haskell Compiler}

YHC was the first to compile Haskell to \js \cite{yhc}. It translated the intermediate Core language to \js (similar to UHC), and had tool support for converting IDL definitions to Haskell, emulation of threading on top of \emph{window.setInterval} and CPS, exception handling, an abstraction layer on top of DOM functionality, and a library for building widgets with inter-widget communication based on \cite{Noble95gadgets:lazy}. As a way to communicate with \js it used a special function called |unsafeJS|, which for example could be used to convert some value to a string:

\begin{code}
unsafeToString a = unsafeJS "return new String(exprEval(a));"
\end{code}

The |unsafeJS| function is passed a string containing a \js expression, where the function parameters are brought into scope in the \js expression under an identical name. The same mechanism was used to implement primitive RTS operations:

\begin{code}
global_YHC'_Primitive'_primIntegerAdd a b = 
  unsafeJS "return exprEval(a) + exprEval(b);"
\end{code}

Where in YHC the runtime evaluation strategy leaks into the FFI, UHC hides it from the programmer through its FEL. Unfortunately, due to amount of work that comes with maintaining a compiler, and the recognition that GHC is the leading Haskell compiler, the authors have decided to discontinue support for the YHC project.

\subsection{GHCJS}

The GHCJS project generates \js based on the STG output it gets by hooking into the GHC compilation pipeline. It appears as if the focus of the project thus far has been mainly on the compilation part, and not so much on the FFI. Its FFI is rather primitive, and piggybacks on the C calling convention:

\begin{code}
foreign import ccall "logResult"
  logResult :: Ptr JSObject -> IO ()
\end{code}

There, however, seems no support for anything else but function calls. Also, being no experts on the possibilities of GHC hooks, we imagine that the decision to overload the C calling convention is born out of necessity. Modifying the GHC front-end to add new syntax will probably require a fork, which is a severe price to pay. This is where the first-class compiler support for a \js back-end, like with UHC, really shines as it provides for maximum flexibility. 

\subsection{Haste}

Haste \cite{haste} was born out of dissatisfaction with the pre-existing Haskell to \js compilers. Similar to GHCJS it hooks into the STG phase of GHC, however it does make quite a few different design decisions. For instance, it chooses to not support concurrency, leave out on-demand code loading, and use a symbolic intermediate between STG and \js to make many simplifications and optimizations possible. 

Haste also uses the C calling conventions for interfacing with \js:

\begin{code}
foreign import ccall foo :: Int -> IO JSString
\end{code} 

The author also shows that the C calling convention can be used to model callbacks. 

\begin{code}
foreign import ccall cb :: (JSString -> IO ()) -> IO ()
\end{code}

However, the programmer needs to be careful when invoking the callback. A lot of RTS details shine through
at this point:

\begin{verbatim}
function cb(callback, _state_of_the_world) {
    A(callback, [[1,'Hello, world!'], 0]);
    return [1, "new state of the world"];
}
\end{verbatim}

Although \emph{dynamic} and \emph{wrapper} can be both implemented using this functionality there is no syntax and automatic wrapping/unwrapping support. Finally, Haste allows external \js dependencies to be included, not based on FFI imports, but simply by providing it as a compilation parameter.

\section{Conclusion, Discussion \& Future Work}

In this chapter we have continued the work of \cite{jcu} by extending the existing infrastructure for programming with the \js FFI. We did this by providing a model for \js types in Haskell, together with type checking and marshalling functions, further we augmented the FFI with a new keyword for creating \js objects, and a simple way to incorporate external \js dependencies. 

There is, however, much work to be done before the \js back-end is ready for prime time. The inefficiencies caused by mismatches in data representation should eventually be solved by the compiler, e.g. for strings this could be done by implementing overloaded strings. Also, the large number of thunks generated by Haskell programs are a major cause performance problems in the web browser. Support for strictness annotations and analyses in UHC would likely improve this situation. Also, more research is necessary into what the best intermediate representation is for generating \js code. The decision to compile from Core to \js has not been made because it is the best match, but for reasons of simplicity. Besides the performance issues it is definitely worth the effort to look into what it takes to support: automatically generated FFI definitions, concurrency, asynchronous server calls, |Data.Dynamic| as library for type checking \js, automatic insertion of type checks based on FFI type annotations, exceptions, and better error reporting.