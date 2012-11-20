\chapter{Introduction}

\section{On a historical note}
\label{ch:researchquestion}

The internet has become a low cost model for delivering content and applications to end-users. Although historically based around a page-centric model there recently has been a shift taking place diverging from latter model to one wherein the page becomes a more active participant in the interaction of the user with the application \cite{ajax}. Essentially taking the page-centric model and transforming it into a more desktop like experience where updates to the graphical user interface appear more or less instantaneous. To take advantage of the high reachability, centralized maintenance, and low distribution costs of the web platform many applications nowadays start out their life as web applications. Furthermore, many traditional applications that could benefit from the web platform are being rewritten such that they can
be deployed on the web. A new class of Internet applications has come to light, often coined as the next generation of Internet applications or \emph{Rich Internet Applications} \cite{allaire:2002}, which try to offer the high level of interactivity that users are accustomed to from desktop applications.

Because the Internet standards were not initially built with this particular usage in mind there are many issues that needed to be resolved to develop  RIAs on top of these standards. Several companies have tried to workaround the limitations by providing the extra functionality through a
browser plugin \cite{ghoda2010introducing,young2008adobe,weaver2007javafx}. However, with the recent resurgence in the amount of browsing platforms and the availability of more adequate standards have rendered these approaches partially redundant and or inadequate. The same capabilities are now either built in natively or can be simulated on top of the new standards \cite{html5}.

Although the capabilities of web browsers have significantly improved they still lack many of
the functionalities that desktop programmers have grown accustomed to. This gap in 
functionality is bridged by the development of graphical user interface (GUI) toolkits that offer things 
like layout and window management, more comprehensive widget sets, and mechanisms to integrate application data.
Accessory to the development of RIAs is the increasing complexity of client-side applications.
The client-side can no longer be regarded as merely a view on the application's data. 
It has gained a multitude of responsibilities such as the management of complex application state, 
synchronization of state with the server,  validation of user input, etc. Consequently the client becomes 
tightly coupled with the server and needs to have in-depth knowledge about its data types and calling conventions. 
This can very quickly become a maintenance problem and a source for bugs when the different tiers (client, server, database) use different formalisms without providing some automated mapping between them \cite{Cooper06links:web}. 
The dynamically typed language JavaScript which is at the foundation of every interactive web application worsens the problem \cite{Mikkonen:2008:WA-:1443226.1444030, Mikkonen:2007:UJR:1698202}. Many people attempted to mitigate the problems by creating new languages \cite{dart,burnham2011coffeescript,cappuccino}, appropriate tooling \cite{bolin2010closure}, libraries \cite{zammetti2009practical,russell2008dojo,lindley2009jquery}, and any combination of these.

\section{Motivation}
\label{sec:webbackendwxHaskell}

The recent addition of a JavaScript back-end to the \emph{Utrecht Haskell Compiler} (UHC) \cite{Dijkstra:2009:AUH:1596638.1596650} 
opens up the possibility to create client-side web applications using Haskell. The compiler translates the UHC Core language (a minimal functional language) to a JavaScript program which may run directly in the browser (supported by a small runtime system).

The use of Haskell will likely induce a performance penalty, but in return offer many advantages over JavaScript: type safety, laziness, compiler optimizations, etc. Furthermore, thinking ahead, when augmented with a Haskell server-side component it is possible to automatically get data type consistency between client and server. This tier-less approach to programming web applications resembles that of the Google Web Toolkit (GWT) \cite{hanson2007gwt} and the proprietary WebSharper \cite{Bjornson:2010:CRG:2050135.2050148}.

% Problem description

To move Haskell forward into the space of web programming, and in particular that of RIAs the presence of tooling for
creating Graphical User Interfaces (GUI) is key. Although Haskell has already quite some existing GUI toolkits \footnote{For a full list of the available GUI libraries in Haskell see \url{http://www.haskell.org/haskellwiki/Applications_and_libraries/GUI_libraries}}, none of them run in the browser. There has been a previous attempt to alleviate this problem, but unfortunately it depends on a proprietary (albeit widespread) browser plugin \cite{wxflashkell}. We have similar goals, but do not want to depend on a proprietary plugin that bypasses the web standards. 

% Interlude

Because there are already many Haskell libraries for constructing desktop GUIs in Haskell it makes sense to retrofit an existing one such that it may run in the web browser. This allows us to benefit from years of experience constructing programming interfaces for GUI development and expedites porting existing desktop applications to the web (typically considered a large undertaking). 

% What are the main approaches

There are two disparate lines of programming GUIs in Haskell: using Functional Reactive Programming (FRP), or the traditional imperative event handler based approach. With FRP widgets (Window Gadgets) are typically viewed as stream processors - taking an input stream and producing an output stream. GUIs are formed by composing widgets using combinators that take two or more widgets and compose them into a single larger widget. The combinators designate how the constituent inputs are routed such that they may become outputs of the larger whole. A GUI application, from the FRP perspective, is therefore often viewed as a network of communicating widgets in which data flow is made explicit. The imperative approach is less explicit about its data flow. A typical imperative-style GUI is constructed by creating new instances of widgets, composing them in a tree-like structure, connecting callbacks to widgets allowing an application to react to event occurrences, and at the end initialize the GUI application by entering an \emph{event loop} that detects events and dispatches them to the appropriate event handlers. This is the more traditional approach to program GUI applications and its lack of explicit data flow makes is much more flexible compared to the FRP approach at the expense of purity and ease of reasoning.

% Compare FRP vs Imperative for suitability

FRP is still active research and is slowly moving out of academia, but has thus far not yet really caught on as a popular way to construct GUIs. There also seems to be no general consensus on which FRP approach is best. On the contrary, the imperative approach has seen wide adoption with many GUIs constructed using it. Furthermore, existing FRP seem to often use advanced language extensions not supported by UHC which would make porting the interface arduous or maybe even impossible. Because of this practical issue and the fact that FRP is not well established as a GUI programming technique we lean towards the safe side and opt for a more imperative approach.

% Oke, but which imperative lib?

The most prominent imperative-style GUI toolkits for Haskell are Gtk2HS \cite{gtk2hs} and wxHaskell \cite{wxhaskell}. Both wrap existing C/C++ GUI toolkits (GTK+ \cite{gtk+}, wxWidgets \cite{smart2006cross}) in Haskell and expose a more abstract interface. Comparatively wxHaskell offers nicer abstractions than Gtk2Hs does. The difficulty with both libraries is that they provide a mere interface to the functionality implemented in a foreign language. Porting any of them to the browser would require the reimplementation of this functionality. Because wxHaskell already works across desktop platforms and offers more evolved abstractions we choose to base our work on wxHaskell. 

\section{Research problem}
\label{ch:problemdefinition}

The absence of an approach for programming client-side web GUIs in Haskell has led us to formulate our problem through the following research question: 

\begin{itemize}
	\item \emph{How can wxHaskell be made to run in the web browser?}
\end{itemize}

In order to provide a suitable answer to this insidiously simple question we will investigate the different options for making wxHaskell run in the web browser, pick one, and demonstrate its viability by porting the implementation of wxAsteroids, a clone of the classic Asteroids game also used by wxHaskell to demonstrate its design and capabilities.

\section{Outline}

The outline of this thesis is as follows. In chapter \ref{chap:background} we provide some background information on GUI toolkits, the architecture of wxHaskell, the web browser platform, and the UHC compiler. Chapter \ref{chap:designspace} explores the different options for porting wxHaskell to the web. After picking a particular implementation path chapter \ref{chap:jsffi} continues with developing the necessary tooling for interfacing with \js from within Haskell. Subsequently, in chapter \ref{chap:lightoo} we develop an OO programming library which we use in chapter \ref{chap:wxasteroids} to implement a subset of wxHaskell necessary for the wxAsteroids game to work. Finally, we wrap up with a conclusion in chapter \ref{chap:conclusion}.