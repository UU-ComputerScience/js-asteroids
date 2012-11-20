\chapter{Background}
\label{chap:background}

\section{On the role of GUI toolkits}

Before GUI toolkits existed every programmer constructed its own interface elements. Obviously the result
was a lack of consistency between different user interfaces and abysmal reuse. This state of affairs
led to the invention of the GUI toolkit to aid consistency and rapid development through reuse. 
Toolkits typically provide well integrated library of standardized widgets and
a framework that deals with the low-level intricacies of graphic manipulation and event handling. The 
application programmer no longer needs to worry about consistency and may reuse existing interface elements. The GUI toolkit is a generic piece of software and as such does not deal with application specific logic. However, through its framework it provides the programmer with the possibility of integrating application specific functions that react to user input.

GUI toolkits have strong roots inside the object-oriented programming language (OOP) community \cite{Krasner:1988:CUM:50757.50759}, one of the many reasons why todays GUIs are more often created in object-oriented languages. Although GUI programming is not particularly
tied to OOP, there is a clear correspondence between OOP concepts and those necessary to effectively model user interfaces. Object identity,
state encapsulation, and inheritance all play an important role in many current GUI toolkit implementations.

\begin{figure}[h]
\center
\includegraphics[scale=.3]{resources/GUIstack.png}
\caption{Unix desktop GUI stack \cite{Gansner:1992:FUI:131302.131315}.}
\label{fig:desktopguistack}
\end{figure}

Figure \ref{fig:desktopguistack} shows where a GUI toolkit is typically situated on a desktop stack.
Although the figure represents Unix-like environments it is quite similar to that of other operating systems.
Libraries like GTK+ \cite{gtk+} and Qt \cite{qt} are situated at the foundational level and communicate
through XLib with the X Window System \cite{x}.

The following sections describe the constituents of a GUI toolkit from a high level perspective. 

\subsection{Graphical representation}

\emph{From an abstract interface description to pixels on the screen.}

\noindent An important part of what GUI libraries facilitate is the composition of an abstract interface description out of widgets.
It is the responsibility of the GUI library to effectively communicate this abstract description to the underlying layer
instructing a display device to display the correct image. Providing a balance between flexibility and standardization of
graphical elements is key to every GUI library. Consequently widgets are typically parameterized over a large class
of attributes (border style, background color, ...) allowing the programmer to influence the style and eventual location of
their widgets. Often, however, the exact placement of widgets is not desirable \emph{per se} and so called layout managers 
may be used that take in many widgets and provide for the automatic alignment of widgets.

By virtue of being a communication mechanism GUIs never exist in isolation but take the role of intermediary between
a human and application. The host operating system may run multiple applications simultaneously 
of which an arbitrary number may use the screen to display their GUIs. The screen thus receives a stream
of possibly interleaved drawing instructions and it suddenly becomes unclear what
the end result will be. For instance, with access to the whole screen applications may erase (partially) each others GUIs by drawing 
at overlapping coordinates. To allow fair use of the display hardware and 
resolve the ambiguity of what will be displayed on screen a central coordination mechanism must be put in place. 

The \emph{X Window System} \cite{x} is such a central coordinating system.
It defines the concept of a \emph{window}, a looking glass through which the user can interact
with a computer program. In practical terms this means that it is a rectangular area on the screen capable of displaying
graphics and receiving input. Every window is contained within another window. The \emph{root window}
is at the top of the window hierarchy and spans the whole screen. The direct children of the root window
are called top-level windows and are treated as special by what is called a \emph{Window Manager}. They are typically
decorated with a title bar, a set of buttons (minimize, maximize, close), and may be moved around and resized.
The window concept helps X with distinguishing applications allowing it to handle clipping \footnote{Clipping is a procedure that identifies if a picture is outside or inside a particular "clipping" area} problems effectively
and implement flexible and efficient event handling mechanisms (see \ref{sec:userinput}).

\begin{figure}[h]
\center
\includegraphics[scale=.6]{resources/Some_X_windows.png}
\caption{A possible placement of windows. \newline \url{http://en.wikipedia.org/wiki/File:Some_X_windows.svg}}
\label{fig:windows}
\end{figure}

On Unix-line operating systems the \emph{X Window System} \cite{Scheifler:1986:XWS:22949.24053} is the most commonly used windowing system 
and many modern GUI toolkits are built on top of it \cite{gtk+,qt}.  It provides a network-transparent window system through a client-server architecture where the server distributes user input to and accepts display instructions from its clients.
The client communicates with the server and \emph{vice versa} through the \emph{X Window Protocol}.
It is however uncommon that this happens directly and most GUI toolkits built on top of the \emph{XLib library} \cite{gettys1990xlib}.
Worthy of mentioning is that the client and server need not be on different machines and the standard situation on a desktop environment
is that both reside on the same machine.

\begin{figure}[h]
\center
\includegraphics[scale=.6]{resources/xlib_helloworld.png}
\caption{A GUI running on Ubuntu 11.10 using XLib see appendix \ref{app:xlib_helloworld}.}
\label{fig:xlib_helloworld}
\end{figure}

Figure \ref{fig:xlib_helloworld} shows the output of a traditional "Hello World" program written using XLib.
What happens is that the application (client) opens a connection to the X server 
and requests the creation of a new top-level window. The request is intercepted by the \emph{Window Manager}
which \emph{reparents} the window such that it becomes a descendant of a decorated window with a title bar
and control buttons. The program proceeds by sending a command instructing the X server to draw the string 
\emph{"hello world"} to the screen. The server delegates the request to the device driver responsible for
managing the graphics hardware.

Now that we almost hit rock bottom we move back up the layers of abstraction and
just above the GUI toolkit we find what are called \emph{User Interface Markup Languages}.
These languages, often dialects of \emph{Extensible Markup Language} (XML), provide a declarative way of specifying a GUI making
it easier to construct GUIs by not bothering the programmer with the distractions of the implementation language.
Furthermore, the implementation of visual construction and manipulation tools becomes arguably
easier. The presence of these tools ushered in a new era in which the programmer is no longer the predominant factor in the process of
constructing GUIs (at least for the graphical part). There is a \emph{scala} of different UI markup languages 
but some of the common ones are: XUL, XAML, and the arguably incomplete XHTML. 

\subsection{User input} 
\label{sec:userinput}

\emph{From a mouse click to a button clicked event}

Typically a user interacts with a computer program through input devices such as a mouse, keyboard, or touch pad.
Upon interaction the device drivers should be aware of changes in device state. How this happens actually depends on
the input device. It may happen through a hardware interrupt or by continuously polling for changes. A GUI registers
interest in device state changes. Once a state change occurs the GUI is notified and it invokes the appropriate
application logic which may perform arbitrary computations and provide the user with visual feedback.
The essence of this process is clearly captured by Fruit \cite{fruit}, expressing
the top level GUI as a Yampa \cite{yampa} signal function (SF):

\begin{code}
type SimpleGUI = SF GUIInput Picture
\end{code}

The \emph{GUIInput} type is a snapshot of the input device state. The \emph{Picture} type contains
the new visual representation in response to the \emph{GUIInput}. Although accurate as
a top level description it is not representative for internal widgets, whom are likely to observe additional
arbitrary values:

\begin{code}
type GUI a b = SF (GUIInput, a) (Picture, b)

SimpleGUI ≅ GUI () ()
\end{code}

In between observing a state change at the device driver level and transforming it into a high level event a lot may happen.
Take for example any GUI toolkit based on top of the X Windowing System. When X receives input it designates the \emph{owner} window.
The window abstraction makes this process efficient and straightforward. 
Each window is assigned a particular part of the screen, stacking order, and focus. Based on these properties X can distill
which windows are visible and thus eligible for receiving events. Dependent on whether the window owner (an application) has registered
interest in the particular event the event will be discarded or dispatched to the application. The application (GUI toolkit) takes notice of these events through what is called an \emph{event loop} which dispatches events to the appropriate event handlers. Event handlers have intimate knowledge of the GUI and may interpret a low level \emph{mouse clicked} event as
a high level \emph{button clicked} event. Although obviously a simplification, leaving out many details, it roughly corresponds to what happens.

\subsection{Application integration}

\emph{From a static user interface to an interactive application.}

Widgets know how to build visual representations out of their abstract descriptions and interpret low level user input as high level semantic actions. To be of any use they allow the integration of application specific logic invoked upon the occurrence of an internal widget event (e.g. button clicked, item selected). These fragments of application specific logic are often supplied through \emph{callback} functions, similar to how the lower level events are captured by the GUI toolkit.
These callback functions, and all other code with access to the application state, may modify the state effectively rendering the widget's visual representation out of sync. The most basic GUI toolkits leave it up to the programmer to manually keep the GUI in sync with the application state.

State synchronization is not only a problem at the level of application integration, but also internally at the level of widget design. To remedy this problem typical implementations of the Model-View-Controller (MVC) pattern use the \emph{Observer Pattern} to keep the view(s) synchronized with the model.

The MVC pattern, first described by Smalltalk \cite{Krasner:1988:CUM:50757.50759}, follows the design principle \emph{Separation of Concerns} to separate out the distinct aspects of widgets - application state (model), visual representation (view), input events (controller) - which makes for a modular GUI library design. The MVC pattern has seen wide spread adoption (albeit in deviating forms) as a technique for developing GUI toolkits \cite{javaswing} \footnote{For a list of MVC frameworks see \url{http://en.wikipedia.org/wiki/Model-view-controller}}. 

The \emph{Observer Pattern} has a strong resemblance with reactive programming techniques wherein a data flow graph is constructed such that changes can be automatically propagated to the data dependencies. 
Reactive programming techniques vary in explicitness about the data flow graph constructed. The Functional Reactive Programming (FRP) approach is typically very explicit about the data flow graph by using arrows to express data dependencies. Explicitness requires more thought, but allows for much better reasoning about what happens as a reaction to a certain event. Further advantages of FRP are that it is much easier to construct composite events (e.g. drap and drop) and because data flow is usually much more granular state changes may cause more efficient repainting.

An alternative approach to state synchronization is \emph{data binding} which under the surface also uses the \emph{Observer Pattern} but typically offers a more end-to-end approach to synchronization. It allows the programmer to create for example a data binding for a text field to a particular record in a database. The data binding will ensure that modifications to the text field's contents will automatically be propagated to the database. 

\section{wxHaskell: a quick overview}
\label{sec:wxhaskell}

wxHaskell \cite{wxhaskell} is a GUI library for Haskell that wraps around the wxWidgets C++ library \cite{smart2006cross} offering a more declarative interface for programming GUIs. It aims to provide a library that is efficient, portable across platforms, retains a native look-and-feel, provides a lot of standard functionality, good abstractions, and is type safe where possible.
The author notes that there are no intrinsic difficulties with achieving these desired properties, but that it takes a large initial effort followed by an enduring maintenance effort. Many previous attempts by the Haskell community to construct GUI toolkits have underestimated the amount of work that goes into maintenance and eventually turned out to fail. To avoid this pitfall wxHaskell builds on top of wxWidgets, a widely supported industrial-strength widget toolkit that eases the development of cross platform GUI applications.

The design of wxHaskell divides into four distinct increasingly abstract layers with at the bottom layer the low-level details of interfacing with C and at the top layer a declarative interface for programming GUIs:

\begin{enumerate}
   \item {\bf wxDirect}: responsible for generating the Haskell wrappers and foreign import declarations from C signatures.
   \item {\bf wxC}: provides the coupling of Haskell with wxWidgets. The wxWidgets FFI declarations are wrapped in Haskell functions that perform conversions between C and Haskell types and the converse.
   \item {\bf wxCore}: uses wxC to expose the core wxWidgets Haskell interface.
   \item {\bf wx}: uses wxCore to provide the user with a more declarative interface for programming wxWidgets. It also contains a combinator library to specify layouts.
\end{enumerate}

In wxHaskell the development of GUI applications is centered around the imperative IO monad, something which in general Haskell programmers would like to avoid. Though because of Haskell's treatment of IO computations as first-class values the library can reach a much higher level of abstraction than can typically be attained in any other language lacking such treatment. Still, wxHaskell employs implicit data flow across event handlers through the use of mutable state.

Inside the bottom layer wxHaskell communicates with wxWidgets by using Haskell's C FFI. In order to retain type safety for widget operations wxHaskell wraps pointers to widgets inside a subtyping hierarchy by using \emph{phantom types}:

\begin{code}
type Object a = Ptr a

data CWindow  a
data CFrame   a

type Window  a  = Object (CWindow a)
type Frame   a  = Window (CFrame a)
\end{code}

Both |CWindow| and |CFrame| are considered phantom types, they lack a corresponding data declaration and thus only exist at compile time. Using type synonyms the subtyping relationship is encoded. Note that the type variable is left polymorphic, however often operations may want to specify that they expect and or produce an exact type:

\begin{code}
frame :: [Prop (Frame ())] -> IO (Frame ())
\end{code}

This is accomplished by applying the type to |()| (unit) making it monomorph. For example in the function |frame|, it takes a list of properties definable on a |Frame| or any supertype and produces an instance of a |Frame|.

Objects encapsulate state and provide methods for state manipulation. A typical pattern is to provide so called \emph{getters} and \emph{setters} for state manipulation. wxHaskell captures association of a particular attribute with a widget using the |Attr| type:

\begin{code}
title :: Attr (Window a) String
\end{code}

A value of type |Attr| bundle both \emph{getter} and \emph{setter}:

\begin{code}
data Attr w a = Attr (w -> IO a) (w -> a -> IO ())
\end{code}

It does not contain any state but simply provides access to an object's accessor functions. Two helper functions are defined that allow both the retrieval of a value and the assignment of a list of values to an object:

\begin{code}
get :: w -> Attr w a -> IO a
set :: w -> [Prop w] -> IO ()
\end{code}

When a value is combined with an attribute it is called a \emph{property}. Properties are represented by the |Prop| type.

\begin{code}
data Prop w = 
      exists a. (Attr w a)   :=   a
   |  exists a. (Attr a w)   :~   (a -> a)
   |  ...
\end{code}

Note the use of existential quantification which allows multiple properties of different value types to be stored in a homogeneous list.
The following example combines all the features described thus far to capitalize the |title| of a |Window|:

\begin{code}

capitalizeTitle :: Window a -> IO ()
capitalizeTitle w = do
   t <- get w title 
   set w [title := map toUpper t]
\end{code}

The definition of |title| is fine because its a fairly unique attribute, however an attribute like |text| is very common and shared by many widgets. Because attributes often overlap and user defined widgets might also want to reuse the same attribute wxHaskell uses type classes to model shared attributes.

\begin{code}
class Textual w where
   text :: Attr w String

instance Textual (Window a) where
   text = ...
\end{code}

Where phantom types provide vertical reuse, \emph{ad hoc} overloading provided by type classes allows for horizontal reuse.
Though wxHaskell has many other features such as layout combinators, event handling, further discussion of features is postponed to the appropriate places in the upcoming chapters.

\section{The target platform}

The web browser has become a complex beast serving as a deployment platform for an ever increasing amount of web pages and applications. In this section we will provide a very brief overview of the core technologies used to built web pages and applications.

\subsection{DOM}

The \emph{Document Object Model} is a cross-platform and language-independent standard for representing and interacting with objects defined in XHTML documents. It is the interface through which web browser expose their internal state of a web page for \js to interact with. 

\subsection{Graphical representation}

Web browsers offer several technologies for rendering graphics, standardized by the World Wide Web Consortium (W3C) \footnote{\url{http://www.w3.org/}}. This section provides a brief overview of the three major standards and describes their individual merits.

\subsubsection{Canvas}

The Canvas is an element in the DOM that can be used by JavaScript to perform basic drawing operations. The drawing operations are performed upon a bitmap surface that has no recollection about what is actually drawn. This makes the canvas applicable for things like animations, image manipulation, games, or anything that draws a large number of objects that are not necessarily interactive. Consequently event handlers maybe attached to the element itself but not to its contents. If such behavior is required it either has to be written from scratch or be simulated by overlaying XHTML elements. Noteworthy is that the Canvas API very much resembles the level of abstraction offered by many well known graphics APIs such as Java2D \footnote{\url{http://java.sun.com/products/java-media/2D/index.jsp}}.

\subsubsection{SVG}

Scalable Vector Graphics (SVG) \cite{svgprimer} is a XML markup language for creating vector graphics. SVG is a strictly higher level drawing facility than the Canvas. It remembers everything it drew inside a scene graph allowing it to be more intelligent in repainting its composites and support interactivity on every object it has drawn, contrary to Canvas. Furthermore, it tightly integrates with the DOM which makes cooperation with other web technologies such as XHTML and CSS easier.

\subsubsection{XHTML - CSS}

Cascading StyleSheets (CSS) apply visual styling to the structure of an XML HyperText Markup Language (XHTML) or to SVG. XHTML roughly defines a set of text elements, elements for attaching semantic meaning to other elements, media elements (video, audio), and a collection of form elements for user input. Each element may be have event handlers attached. The combination XHTML - CSS offers more in terms of standard facilities compared to Canvas and SVG but lacks the flexibility of both in terms of flexible drawing primitives. This deficiency is usually compensated by either using browser plugins, or more recently by embedding Canvas and SVG elements in XHTML documents augmenting the XHTML experience \footnote{A hybrid HTML5 game, Canvas, XHTML, and CSS \url{http://www.cuttherope.ie/}}.

\subsection{Interactive web pages with JavaScript}

JavaScript/ECMAScript is a dynamically typed prototype-based language. It is the primary means for turning static web pages into highly interactive web applications. The web browser (the host environment) allows access to the elements of a web page by exposing its functionality to JavaScript through the Document Object Model (DOM). JavaScript features objects, first class - higher order - and variadic functions amongst other things \cite{ecmascript}.

\section{UHC}

\begin{figure}[h]
\center
\includegraphics[scale=.4]{resources/uhc_pipeline.png}
\caption{The UHC pipeline.}
\label{fig:uhcpipeline}
\end{figure}

The Utrecht Haskell Compiler is an experimental compiler for Haskell 98 plus some additional language extensions, developed at the University of Utrecht 
\cite{Dijkstra:2009:AUH:1596638.1596650}. Its purpose is more geared towards being a language experimentation platform as opposed to being a industrial-strength compiler like the Glasgow Haskell Compiler (GHC) \cite{ghc}. Internally the compiler is organized into different language variants. Each variant may be compiled separately making language experimentation significantly easier. The compilation proceeds by pushing a Haskell through a series of transformations, expressed as algebras using the Utrecht University Attribute Grammar system (UUAG) \cite{Swierstra:1998:Combinator}, which result in intermediate languages that get progressively closer to the target platform. 

Figure \ref{fig:uhcpipeline} displays the compilation pipeline targeting JavaScript. In the first phase the input Haskell program is desugared into Essential Haskell (EH) a desugared variant of Haskell. EH is transformed into the Core language which constitutes a very minimal functional language resembling the lambda calculus. Subsequently the JavaScript backend hooks into the compilation process and translates Core to JavaScript. It links the compiled source program together with its base library dependencies and the \emph{Runtime System} (RTS) inside a single XHTML document.