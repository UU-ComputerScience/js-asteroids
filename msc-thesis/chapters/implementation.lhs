\chapter{wxAsteroids in the web browser}
\label{chap:wxasteroids}

In this chapter we will put the results from the previous two chapters to use by implementing a subset of wxHaskell that runs inside the web browser. The wxHaskell paper \cite{wxhaskell} explained its design and capabilities by implementing a clone of the classic asteroids game \emph{wxAsteroids} \cite{wxasteroids}. It is a great showcase of the different key aspects of \emph{wxHaskell}: widgets, graphics rendering, and user input. Furthermore, it provides a good example of how typical \emph{wxHaskell} programs are constructed. Instead of porting the fully featured wxAsteroids we have ported a less feature heavy version due to time constraints. Figure \ref{fig:wxaster} shows the original \emph{wxAsteroids} on the left, running on the desktop, next to our port running on the desktop (middle), and in the web browser (right). It shows how LightOO together with the \js FFI can be used to implement the wxWidgets OO design in Haskell in terms of the technologies available in the browser. 

In this chapter we will explain the gist of \emph{wxAsteroids}, the design issues, followed by a more detailed explanation of the implementation.

\begin{figure}[h]
\center
\includegraphics[scale=.3]{resources/WxAsteroids.png}
\includegraphics[scale=.3]{resources/ubuntu_wxasteroids.png}
\includegraphics[scale=.3]{resources/browser_wxasteroids.png}
\caption{The original wxAsteroids on the desktop (left), modified wxAsteroids on the desktop (middle), and in the web browser (right).}
\label{fig:wxaster}
\end{figure}

\section{wxAsteroids}

In the \emph{asteroids} game the player is tasked with carefully maneuvering its spaceship through an asteroid field making sure it does not get hit. The spaceship can move left and right using the arrow keys. There is an inifite supply of asteroids that move vertically downwards. Whenever a rock hits the spaceship, the rock turns into an explosion. In accordance with the original wxAsteroids hitting a rock does not destroy the spaceship. First, we define some constants:

\begin{code}
height    = 600 
width     = 300 
diameter  = 24 
chance    = 0.1
\end{code}

The |height| and |width| values determine the dimensions of the game field. The |diameter| represents the diameter of a rock, and the |chance| determines the chance a new rock appears in a given time frame. The |asteroids| function constructs the user interface, and is run by the |start| function: 

\begin{code}
asteroids :: IO () 
asteroids = do
    vrocks  <- varCreate randomRocks
    vship   <- varCreate $ div width 2

    w       <- window Nothing []

    t       <- timer w  [ interval    := 50
                        , on command  := advance w vrocks
                        ]
    
    set w  [  area         := rect (pt 0 0) (sz width height)
              on paint     := draw vrocks vship 
           ,  on leftKey   := varUpdate vship  (\x -> max  0      (x - 5))  >> return ()
           ,  on rightKey  := varUpdate vship  (\x -> min  width  (x + 5))  >> return ()
           ]

main = start asteroids
\end{code}

First two \emph{mutable} variables are created: |vrocks| holds an infinite list containing the positions
of all the future rock positions, |vship| contains the current position of the spaceship. 

Next, we create a top-level window that serves as a placeholder for the game. The first parameter denotes a potential parent window, the second a list of properties. Subsequently we attach a timer to the window firing every 50 milliseconds. On each tick, it calls |advance|, moving all rocks to their next position and updating the screen.

Finally, we set a few attributes on the window |w|. We assign it an |area| with the given constant dimensions. The other attributes are prefixed with |on| designating event handlers. The |paint| event handler is invoked when a repaint request is made, and draws the current game state to the screen through |draw| (later defined). Pressing the left or right arrow key changes the \emph{x} position of the spaceship.

The |vrocks| variable contains an infinite list of all future rock positions. This infinite list is generated by the |randomRocks| function which depends on random number generation. Because at the time of writing there was no back-end support for random number generation through the standard |System.Random| library we used a more \emph{ad hoc} solution:

\begin{code}
foreign import js "Math.random()"
  randomNumber :: IO Double

rand _ = unsafePerformIO randomNumber

randoms :: [Double]
randoms =
  let inf = undefined : inf
  in map rand inf
\end{code}

The |randoms| function provides a infinite list of random numbers in the range [0,1). It works by mapping a random number generator |rand| over an infinite list. Note that this only works because |rand| is not subject to let floating. 

\begin{code}
randomRocks = flatten [] (map fresh randoms)

fresh r 
  | r > chance = [] 
  | otherwise  = [track (floor (fromIntegral width * r / chance))] 

track x = [point x (y - diameter) | y <- [0, 6 ... height + 2 * diameter]]

flatten rocks (t : ts) = 
  let  now    = map head rocks 
       later  = filter (not . null) (map tail rocks) 
  in now : flatten (t ++ later) ts 
flatten rocks [] = error "Empty rocks list not expected in function flatten"
\end{code}

The |fresh| function is mapped over |randoms|. It compares each number against the |chance| constant,
and if a rock should appear it generates a finite list of future rock positions that move the rock from the top
to the bottom of the screen, otherwise it returns the empty list. Finally, the |flatten| function flattens this list into a list of time frames, where each element contains the position of every rock in that particular time frame.

The |advance| function is called on every timer tick:

\begin{code}
advance vrocks w = do 
  (r : rs) <- varGet vrocks 
  varSet vrocks rs
  repaint w
\end{code}

It moves |vrocks| to the next time frame (its tail), and request a repaint of the window (|w|). 
A repaint causes the |paint| event handler to be triggered which in turn calls |draw| with two parameters: the \emph{graphics context} (|gc|) and view area (|view|). The graphics context paints on the window area on the screen, but is in principal independent of its back-end. 

\begin{code}
draw vrocks vship gc view = do
  rocks  <- varGet vrocks 
  x      <- varGet vship 
  let
    shipLocation  = point  x (height - 2 * diameter)
    positions     = head   rocks
    collisions    = map    (collide shipLocation) positions
    
  drawShip gc shipLocation
  mapM (drawRock gc) (zip positions collisions) 
\end{code}

The |draw| function reads the current rock and spaceship positions, and positions the spaceship at the current x, and fixed y-position. Then it checks if there are any \emph{collisions} between the spaceship and any of the rocks. Finally, we draw the spaceship and all the rocks onto the screen. The |collide| function simply checks whether a rock has entered the spaceship's comfort zone given their \emph{positions}.

\begin{code}
collide pos0 pos1 = 
  let distance = vecLength (vecBetween pos0 pos1) 
  in distance <= fromIntegral diameter
\end{code}

Both entities are drawn using the |drawBitmap| function. It takes a graphics context, bitmap, position, transparency mode (not implemented), and a list of properties as arguments. Dependent on whether a collision occured the picture of a rock changes to either a normal rock or a exploded one.

\begin{code}
drawShip gc pos = drawBitmap gc ship pos True [] 

drawRock gc (pos, collides)= 
  let rockPicture = if collides then burning else rock
  in drawBitmap gc rockPicture pos True []
\end{code}

Finally, we specify the resources that we used.

\begin{code}
rock     = bitmap "rock.ico"
burning  = bitmap "burning.ico"
ship     = bitmap "ship.ico"
\end{code}

The summary we just gave does not differ much from the one given in \cite{wxhaskell}. We have simplified the porting effort by omitting features such as the menu, sound effects, and status field. Furthermore, we render the game on top of a |window| instead of a |frame| (which features all the standard decorations such as a title, maximize, minimize, and closing buttons). Besides the ommittances the source has remained largely the same. 

\section{Design}

\subsection{Approach}

We took wxHaskell version 0.12.1.4 from hackage and performed a depth first search on the features that
needed to be supported in order for wxAsteroids to work. All unused features were commented out and all irrelevant parts that dealt with the implementation details of the C++ back-end were removed. Furthermore, all relevant functions exposed through wxcore were undone from their implementation and replaced by |error "to be implemented"|. This allowed us to type check the whole codebase without yet having the implementation at hand. At all times we kept our codebase compatible with \emph{ghci} by using CPP macros to conditionally import modules. With hindsight this turned out to be well worth the effort. Due to its superior error reporting capabilities we could more easily pinpoint programming errors. It also served as a useful reference for improving UHC by occassionally catching errors in its implementation. 

Fortunately, none of the implementation details of \emph{wxcore} leak to \emph{wx}. This allowed us to leave the \emph{wx} sources largely untouched. We used the elaborate wxWidgets documentation and source code as a reference for implementing wxcore. 

\subsection{Objects}

In order to maintain some type safety when communicating with C++, wxcore assigns phantom types to objects, with as top-level type:

\begin{code}
data Object a  =  Object   !(Ptr a)
               |  Managed  !(ForeignPtr (TManagedPtr a))
\end{code}

All objects are either a normal or managed pointer to an object that lives in the C++ world. For example, the type of a window (|Window a|) is a type synonym that expands to |Object (WxObject (CEvtHandler (CWindow a)))|. The type structure used here is identical to the type structure we used to program OO in Haskell. This turns out the be very useful because it allows the implemention of wxcore functions without (in most cases) altering their interface which makes it very close to a drop-in replacement for the original wxcore implementation.

The |Object| data type is replaced with a record |IObject| with a corresponding implementation: 

\begin{code}
object = clazz $ \tail self -> do
   flag <- newIORef False
   return IObject {
       setFlag      = writeIORef flag
      ,getFlag      = readIORef flag
      ,_objectTail  = tail
   }
\end{code}

In \emph{Graphics.UI.WXCore.Types} there are quite some methods defined for objects with the assumption that they are implemented as pointers to C++ objects. Some of these methods are replaced whereas others do not make sense anymore. For example, |objectCast| is replaced by |upcast| and |downcast|, |objectIsNull| makes no sense anymore as we always have evidence that an object exists there is no need to check whether it is null, |objectDelete| is also irrelevant we can simply rely on garbage collection.

What has also changes is object identity. Before, object identity boiled down to pointer equality. The idiomatic Haskell approach to test values for equivalence is by structural equivalence. However, objects are black boxes that hide their data. What makes two objects identical becomes subject to interpretation of the programmer. Hence, we implement an equality test similar to pointer equality \cite{tpl}:

\begin{code}
sameObject a b = do
   a # setFlag $ False
   b # setFlag $ True
   a # getFlag
\end{code}

Every object has a |flag| associated with it. The |sameObject| function tests if changing it in |a| also changes it in |b| essentially testing pointer equality.

\subsection{Organization}

We have set-up wxcore such that every class definition resides in its own module in similar to header files in C++. Implementations of a particular class import the interface definition and re-export it together with a class for constructing instances. Interface definitions typically need other interface definitions to define their type. This is no problem unless both definitions depend on each others types. Unfortunately, this is a quite common scenario in OO languages. Since Haskell does not support cyclic imports there is no other option than placing the interface definitions in the same module breaking the organization scheme. 

\subsection{Mapping to the web browser}

The wxcore abstractions at some point need to interface to the target platform. The web browser already offers a great deal of functionality that make it easy create GUIs. We choose the easy route by  piggybacking on much of the high-level technology is already present. We let a wxWidget window correspond to a HTML div element. Asteroids draws on a graphics context associated with a window. As back-end for the graphics context we use a HTML5 canvas element, because drawing on divs is not possible without CSS3 hacks. The graphics context is created on demand and covers the whole window. We could just as well have used SVG as a back-end, but it was easier to use the canvas due to its small API.

Event listeners are registered on the div representing the window. When an event is triggered by the target platform its event object properties are read and mapped to a subclass of the \emph{Event} object. This object is then dispatched internally inside the wxWidgets event system which invokes to the appropriate event listeners. Decoupling the native event mechanism from the wxWidgets event mechanism provides the opportunity to trigger custom events not originating from the target platform. 

Finally, initialization of the application is done through the |start| function:

\begin{code}
start io = 
  w   <- htmlWindow
  -- Wrap a haskell function as if it were a regular JavaScript function
  cb  <- wrapFunc io
  -- Set the onload event on the window object
  set "onload" cb w
  return ()
\end{code}

The moment the web page is loaded the application starts. 

\section{Implementation details}

\subsection{Subtyping}

WxHaskell makes extensive use of a phantom type structure for modelling a type safe interface to foreign objects. In chapter 5, section \ref{subsubsec:changeinsemantics} we explained that functions consuming abstract objects (i.e. objects with their tail left polymorph) are inhibited from using casts on those objects. Because there are many places where wxHaskell uses polymorphic objects - for attribute classes, as function arguments, inside properties - it make sense to explore whether this can persist if we provide a Haskell implementation, and if changes are required how this affects the interaction of the different parts of wxHaskell.

We start by looking at the \emph{Graphics.UI.WX.Classes} module, part of the \emph{wx} library. It defines a host of type classes for capturing common attributes of widgets. For example, there is a type class that ranges over widgets that can be positioned and sized:

\begin{code}
class Dimensions w where
  area      :: Attr w Rect
  ...
\end{code}

Typically, these attribute classes are instantiated by wxHaskell with polymorphic objects:

\begin{code}
instance Dimensions (Window a) where
  ...
\end{code}

Fortunately, this part of wxHaskell is not affected by our implementation because it turns out that in all encountered cases the concerning widget is used solely as the first argument for invoking its own methods. Nevertheless it would have been possible to use these type classes if we were forced to instantiate them with concrete objects, there would simply be more explicit casting compensating for the loss in expressiveness in the types.

However, we have encountered cases wherein we were forced to change the type of a function. For instance, in the case of event processing wxHaskell invokes the |evtHandlerProcessEvent| method passing it any subtype of the \emph{Event} class.

\begin{code}
evtHandlerProcessEvent :: EvtHandler a -> Event b ->  IO Bool
\end{code}

Because an \emph{Event} object ends up as an argument to a callback function, which typically needs to know the event type (e.g. a mouse or keyboard event), there will be some casting involved. Hence we are forced to change the function type making it accept only concrete \emph{Event} objects. 

\begin{code}
evtHandlerProcessEvent :: EvtHandler a -> Event -> IO Bool
\end{code}

Another thing we encountered that sometimes proves useful is to be able to change the type of property (|Prop w|), e.g. from a |Prop Frame| to a |Prop Window|. The \emph{wx} library already defines casting functions for |Attr w a| and |Prop w|:

\begin{code}
castProp :: (v -> w) -> Prop w -> Prop v
castProp coerce prop = 
  case prop of
    (attr := x)   -> (castAttr coerce attr) := x
    (attr :~ f)   -> (castAttr coerce attr) :~ f
    (attr ::= f)  -> (castAttr coerce attr) ::= (\v -> f (coerce v))
    (attr ::~ f)  -> (castAttr coerce attr) ::~ (\v x -> f (coerce v) x)

castAttr :: (v -> w) -> Attr w a -> Attr v a
castAttr coerce (Attr name getter setter upd) = 
  Attr  name  (\v -> getter (coerce v)) (\v x -> (setter (coerce v) x))
              (\v f -> upd (coerce v) f) 
\end{code}

Both functions take a coercion function and consistently apply it to the argument (contravariant) positions. The |Prop w| type is actually a contravariant functor with |castProp| as mapping function, because its type parameter |w| is only used in contravariant positions.

\begin{code}
class Contravariant f where
   contramap :: (a -> b) -> f b -> f a

instance Contravariant Prop where
  contramap = castProp
\end{code}

We specialize |contramap| for upcasting the properties:

\begin{code}
upcastProp :: forall v w. w :>: v => Prop v -> Prop w
upcastProp p = 
  contramap (handleErr . (downcast :: w -> Maybe v)) p
  where
  handleErr = maybe (error $ "Non-existent property: " ++ propName p) id
\end{code}

That the |upcastProp| function is implemented in terms of |downcast| may seem somewhat counterintuitive. Upcasting a property does not change the getter and setter stored in the attribute, but wraps them inside a new function  accepting the upcasted type which is subsequently coerced back (downcasted) to the old type and applied to the wrapped function. Similarly, we define its dual:

\begin{code}
downcastProp :: forall v w. w :<: v => Prop v -> Prop w
downcastProp p = contramap (upcast :: w -> v) p
\end{code}
 
\subsection{Interfacing with the DOM}

For platform dependent features the implementation needs to communicate with \js. In chapter \ref{chap:jsffi} we developed some techniques which we are now going to use for interfacing with \js, in particular the DOM. The DOM is the primary interface to browser functionality. Its interfaces are specified in IDL (Interface Definition Language), here follows an excerpt of the \emph{HTMLElement} IDL definition\footnote{\url{http://www.whatwg.org/specs/web-apps/current-work/multipage/elements.html\#htmlelement}}:

\begin{verbatim}
interface HTMLElement : Element {
  attribute DOMString title;
  ...
  readonly attribute boolean isContentEditable;
  ...
  [TreatNonCallableAsNull] attribute Function? onkeydown;
  ...
}
\end{verbatim}

The top-level declaration provides a name for the interface, optionally followed by a colon and the interface it extends from. The body contains all attributes annotated with their contained type, additional constraints on the interpretation of the contained data (\emph{TreatNonCallableAsNull}), and its usage (\emph{readonly}).

To model the interface types we simply extend |JSObject|:

\begin{code}
data CHTMLElement a
type HTMLElement_ a = Element (CHTMLElement a)
type HTMLElement = HTMLElement_ () 

data CElement a
type Element_ a = Node_ (CElement a)
type Element = Element_ ()

data CNode a
type Node_ a = JSObject (CNode a)
type Node = Node_ ()
\end{code}

The structure is exactly the same as the one used by wxHaskell to model C++ object types in Haskell. It is even a better fit for \js, because its single prototype chain does not naturally allow the modeling of multiple inheritance, of which it is known that it cannot be practically modeled with this type structure \cite{Fluet:2006:PTS:1180085.1180088}.

For wxAsteroids it is important to know when the user holds down either the left or right key. We import the \emph{onkeydown} event using the |eventJSRef| function:

\begin{code}
onkeydown  = eventJSRef "onkeydown"
\end{code}

It captures the onkeydown property as a read-write |JSRef|, interprets the \emph{TreatNonCallableAsNull} constraint as a |Maybe|, and can be used to import any of the event handlers that are part of an \emph{HTMLElement}. 

\begin{code}
eventJSRef :: String -> HTMLElement_ a -> JSRef ReadWrite (Maybe (Event -> IO ()))
eventJSRef id e = 
  let get = do
        f <- getProperty id e
        if isNull f 
          then return Nothing
          else unwrapFunc1 (unsafeCoerce f)

      set Nothing   = setProperty_ id _null e
      set (Just f)  = do
        f <- wrapFunc1 f 
        setProperty_ id f e

  in newJSRef get set
\end{code}

Internally it creates a new |JSRef| with a getter and setter that respectively read and write the property. Because there is no automatic back and forth conversion from Haskell functions to \js functions we use |wrapFunc1| and |unwrapFunc1| for respectively wrapping and unwrapping 1-argument functions. These functions are part of a larger family of wrapping and unwrapping functions:

\begin{code}
foreign import js "wrapper"
  wrapFunc :: IO () -> IO (JSFunction (IO ()))

foreign import js "wrapper"
  wrapFunc1 :: (a -> IO ()) -> IO (JSFunction (a -> IO ()))

foreign import js "dynamic"
   unwrapFunc :: JSFunction (IO ()) -> IO (IO ())

foreign import js "dynamic"
   unwrapFunc1 :: JSFunction (a -> IO ()) -> IO (a -> IO ())

...
\end{code}

Unfortunately, the wrapping and unwrapping is not tracked by the RTS. Repeated getting and setting will grow a series of wrapping and unwrapping functions around the original function causing performance problems. Also, the use of Haskell strings for accessing \js properties is a root of performance problems as well. Strings need to be packed before the are in \js format requiring order $n$ time where $n$ is the length of the string. A better solution would be to use overloaded strings\footnote{\url{http://www.haskell.org/ghc/docs/7.4.2/html/users_guide/type-class-extensions.html\#overloaded-strings}} with compiler support for representing Haskell strings directly as \js strings. Besides the technical details |eventJSRef| is a definite improvement upon the \emph{laisser faire} attitude of \js where a |TreatNonCallableAsNull| constraint depends run-time type checking, the event handler function can be any type of function making it very easy to let programming errors go by unnoticed.



\subsection{Implementing wxTimer}

We discuss the implementation of the \emph{wxTimer} object, a small and relatively self-contained example that touches many of the aspects discussed thus far. We start at \emph{wxAsteroids} moving stepwise from \emph{wx} to the \emph{wxcore} implementation.

First, in \emph{wxAsteroids}, we create a timer and attach it to a \emph{Window}. We set the timer such that it calls |advance| every 50 milliseconds.

\begin{code}
...

t  <- timer w  [  interval    := 50
               ,  on command  := advance vrocks w
               ]

...
\end{code}

The |timer| function is defined in |Graphics.UI.WX.Timer|, part of the \emph{wx} package:

\begin{code}
type Timer  = TimerEx ()

timer :: Window a -> [Prop Timer] -> IO Timer
timer parent props
  = do t <- windowTimerCreate parent
       timerStart t 1000 False
       set t props
       return t
\end{code}

It creates a new timer using |windowTimerCreate|, sets the default resolution to 1 second, and sets some properties on the object. The |interval| attribute is specific to a timer object, and hence has its widget type fixed to a concrete timer.

\begin{code}
interval :: Attr Timer Int
\end{code}

The |command| attribute is overloaded on the widget type as it can be reused by other widgets for setting zero-argument event handlers.

\begin{code}
class Commanding w where
  command :: Event w (IO ())

instance Commanding Timer where
  command = newEvent "command" timerGetOnCommand timerOnCommand
\end{code}

Before we move on the the \emph{wxcore} implementation, we only need to make a minor adjustment to the type signature of |timer| to account for name mismatch:

\begin{code}
timer :: Window_ a -> [Prop Timer] -> IO Timer
\end{code}

In \emph{wxcore} we find the definitions for |timerGetOnCommand|, |timerOnCommand|, etc. 
These functions are implemented with the C++ back-end in mind. For example, the |timerGetOnCommand| and |timerOnCommand| functions rely on some C++ wrapper code that allows storing and retrieval of Haskell closures. It ensures that Haskell functions can be safely passed into the C++ world without Haskell garbage collecting them.

\begin{code}
timerOnCommand :: TimerEx a -> IO () -> IO ()
timerOnCommand timer io
  = do closure <- createClosure io (\ownerDeleted -> return ()) (\ev -> io)
       timerExConnect timer closure

timerGetOnCommand :: TimerEx a -> IO (IO ())
timerGetOnCommand timer
  = do closure <- timerExGetClosure timer
       unsafeClosureGetState closure (return ())
\end{code}

Obviously, these implementations make no sense in our situation. We leave the function types in tact, but reimplement their functionality in terms of calls to a timer object. Before we present their new implementation we first provide a sample of the C++ implementation of the timer object:

\begin{verbatim}
class wxTimerBase : public wxEvtHandler
{
  public:
    wxTimerBase(wxEvtHandler *owner, int timerid = wxID_ANY) { 
      Init(); 
      SetOwner(owner, timerid); 
    }

    void SetOwner(wxEvtHandler *owner, int timerid = wxID_ANY) {
        m_owner = owner;
        m_idTimer = timerid == wxID_ANY ? wxWindow::NewControlId() : timerid;
    }
    
    int GetInterval() const { return m_milli; }
    bool IsOneShot() const { return m_oneShot; }
    ...
\end{verbatim}

A timer inherits from \emph{wxEvtHandler}, and is constructed by passing it an owner and optionally an identifier. Normally identifiers are used by wxWidgets to identify windows, but because we could not infer the use case of ids on a timer we left it out of our implementation.

\begin{code}
timer owner =
   (timer' `extends` evthandler) noOverride set_EvtHandler_Tail
   where
   timer' tail super self = do 
      ...
\end{code}

The constructor now corresponds to the |timer| function (different from the |timer| defined inside \emph{wx}), and extends from |evthandler|, which we will not present for the sake of brievity. Inside the constructor some variables are brought into scope covering both the \emph{Init} and \emph{SetOwner} function:

\begin{code}
      interval   <- newIORef 0
      owner      <- newIORef owner
      jsTimerId  <- newIORef (-1)
      isone      <- newIORef False
      isRunning  <- newIORef False
      return ITimer {
        ...
\end{code}

Most variables have corresponding methods for getting their values. 

\begin{code}
        ,_timerGetInterval  = readIORef interval 
        ,_timerGetOwner     = readIORef owner
        ,_timerIsOneShot    = readIORef isone
        ...
\end{code}

The |timerStart| method implements the functionality for starting a timer with a particular frequency (|milli|), and provides the possibility of firing the timer only once (|oneshot|).

\begin{code}
         ,_timerStart        = \milli oneshot -> do
            
            let this = upcast self :: Timer
            
            timingEvent  <- new $ timerEvent this
            handler      <- readIORef owner

            let cb = do {
               ; handler # evtHandlerProcessEvent $ (upcast timingEvent)
               ; when oneshot (self # timerStop)
            }

            w        <- htmlWindow
            timerId  <- setInterval w cb milli
            writeIORef jsTimerId timerId
            return True
\end{code}

The implementation makes fruitful use of the native |setInterval| function for installing a timed callback on the global window object. Inside the callback the |evtHandlerProcessEvent| is invoked on the owning object passing it an instance of a \emph{TimerEvent}. If the timer is a one shot than it stops the timer from preventing any future invocations of the callback. The |setInterval| method returns an identifier which we store inside the |jsTimerId| variable such that we may later use it to stop the timer:

\begin{code}
         ,_timerStop = do
            timerId <- readIORef jsTimerId
            clearInterval timerId

         ,_timerTail = tail
      }
\end{code}

With the timer implementation we can now implement the |timerOnCommand| and |timerGetOnCommand| functions, which are no methods of the timer class, but helper functions created by wxHaskell.

\begin{code}
timerOnCommand :: Timer_ a -> IO () -> IO ()
timerOnCommand t f = do
   owner <- t # timerGetOwner
   (owner # evtHandlerBind) wxEVT_TIMER (const f) idAny idAny

timerGetOnCommand :: Timer_ a -> IO (IO ())
timerGetOnCommand t = do
   owner <- t # timerGetOwner
   cb <- do { cd <- (owner # evtHandlerGetHandler) wxEVT_TIMER idAny idAny
            ; return $ maybe (const $ return ()) id cd
            }
   return $ cb (error "touched event object")
\end{code}

The |timerOnCommand| function simply binds a callback to the owner of the timer, whereas |timerGetOnCommand| 
tries to retrieve an already bound callback. The |evtHandlerGetHandler| method had to be invented as its not part of the wxWidgets \emph{EvtHandler} class, which can be justified by the fact that wxHaskell also uses wrapper code to implement this functionality. 

Finally, the |windowTimerCreate| function simply instantiates a new timer. 

\begin{code}
windowTimerCreate :: Window -> IO Timer
windowTimerCreate w = new $ timer (upcast w)
\end{code}

We should note that we have changed the type signature such that it takes a concrete window instead of a polymorphic one. We could have left it polymorph, because the owner object is never required as a concrete object inside the timer implementation, but doing this would have required that we made use of parameterized classes in effect complicating the types; we choose not to.  

The wx timer now works inside the web browser, completely transparent to the end-user.

\section{Conclusion}

We have successfully ported a feature-light version of wxAsteroids to the web browser. Albeit the modest scope of the port we foresee no intrinsic difficulties in implementing the lacking features. We have discussed the design decisions involved and provided some details on the actual implementation. However, there is still much work to be done making decisions on how to best map wxWidgets features onto the web platform. Also, programming in LightOO feels a bit hacky due to the lack of an uniform treatment of subtyping which has forced us to make slight modifications to the wxcore interface. Furthermore, the lack of recursive modules breaks code organization and we expect that a full implementation of wxWidgets will soon run into performance problems related to the \js back-end.  