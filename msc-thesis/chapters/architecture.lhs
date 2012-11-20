\chapter[Exploring the design space]{wxHaskell for the web: exploring the design space}
\label{chap:designspace}

\emph{Bridging the wxWidgets gap}

The goal of this thesis is to show how a subset of wxHaskell can be ported to the web browser. However, as always, there are many routes that lead to Rome. In section \ref{sec:wxhaskell} we briefly discussed the layered architecture of wxHaskell. Before we pick a particular implementation path we first consider the different ways we can cut the cake. Because wxWidgets is written in C++ and all its implementations are in terms of desktop technology there is no chance of reusing any code. Without wxWidgets, wxHaskell is just a small layer of abstractions. Somehow the gap left by wxWidgets needs to be filled with an implementation in terms the browser technology whilst trying to maintain the old programming interface. In figure \ref{fig:wxweboptions} the different options are aligned next to the original wxHaskell set-up. They only differ in where the line is drawn separating Haskell from \js. In this chapter we will discuss each option individually, weigh their pros and cons, and pick one approach that will dictate the further developments in this thesis.

\begin{figure}[h]
\center
\includegraphics[scale=.5]{resources/mapping.png}
\caption{Design options: orange: C++, yellow: JavaScript, blue: Haskell}
\label{fig:wxweboptions}
\end{figure}

\section{Port wxC in Haskell (A)}

The intention of this approach is to leave the original \emph{wxC} interface intact, swap its implementation with equivalent functionality provided by some \js GUI toolkit, augmented with some additional \js wrapper code to overcome mismatches in functionality. With the \js GUI toolkits maturing there are quite some options that provide approximately the same functionality as wxWidgets. 

In order to access all the features of the underlying \js GUI toolkit there is a significant amount of boilerplate code required. Assuming that we may generate this boilerplate automatically from an API description there still remains lots of subtle porting work in order to nicely fit the functionality with the \emph{wxC} interface. From a Haskell perspective we end up in a rather strange situation wherein we are trying to sustain an imperative programming interface for an C++ API by porting its calls to a \js API inside Haskell which does not natively support OO programming. Furthermore, the obtained solution will from a Haskell point of view not be very portable as the majority of the functionality is still implemented in a foreign language. Also, changing to another GUI toolkit requires a total rewrite.

{\bf Advantages:}
\begin{itemize}
   \item We get a large tested and maintained code base almost for free;
   \item Ideally, it is a drop in replacement for \emph{wxC} requiring no change in the above layers.
\end{itemize}

{\bf Disadvantages:}
\begin{itemize}
   \item Connecting two OO interfaces inside Haskell will not result in idiomatic Haskell;
   \item The majority of the functionality is still written in \js and thus cannot benefit from compiler optimizations;
   \item Tight coupling to \js GUI toolkit results in poor portability and a high degradation risk;
   \item Poor extensibility from within Haskell.
\end{itemize}

\section{Port wxC in \js (B)}

Option B draws the line between Haskell and \js a little bit further down. The key idea is to port the wxWidgets API to \js and perform the actual implementation in \js. This has as advantage over \emph{A} that it leads to a much more natural implementation. It does leave open the question whether there exists a reasonable semantics preserving mapping from C++ to \js. The fact that most web browsers implement the DOM in C++ and provide a \js API to access its functionality would suggest that there is. Also, the similarity between the wxWidgets \js API and its C++ version will likely ease the implementation of the Haskell interface code. This can be explained by the nature of the mapping which will necessarily depend on the set of language features formed by lowest common denominator of C++ and \js, ruling out the use of idiosyncratic features of \js which make it particularly hard to bolt a type safe Haskell interface on top. 

With a set-up much like wxHaskell it inherits many of its architectural properties. For instance, the \js code base does not take part in the compilation process which complicates linking and optimization. This might lead to suboptimal code and larger binaries. Furthermore, every interaction with the wxWidgets implementation induces cross language communication inflicting a performance penalty. In wxPython (a Python wrapper for wxWidgets) widgets are extensible through inheritance just like in C++. However, in Haskell there is no direct equivalent to inheritance. This will eventually limit the flexibility of the end-user gets when using the library.

{\bf Advantages:}
\begin{itemize}
   \item Resembles the wxHaskell approach;
   \item Avoids many difficulties of interfacing with \js from Haskell by providing a C++ like interface in \js;
   \item See advantages of option A.
\end{itemize}

{\bf Disadvantages:}
\begin{itemize}
   \item The majority of the library is implemented in \js;
   \item Not portable from a Haskell perspective;
   \item Not easily extensible from within Haskell.
\end{itemize}

\section{Replace wxCore with a Haskell implementation (C)}

We can also try to move the wxWidgets implementation as much as possible into Haskell thereby reducing the platform dependent code to a mininum greatly improving portability. With a major part of the code base in Haskell we may reap all of its benefits such as compiler optimizations, type safety, etc. Though implementing wxWidgets in Haskell is easier said than done. To stay true to wxWidgets we should port is OO design to Haskell, because Haskell was not envisioned as a OO language with typical features like: subtyping, inheritance, encapsulation; it is not clear at all to us whether this is even possible without extending the language. Fortunately, the authors of \oohaskell have shown that Haskell can indeed be used to model the typical, and some of the more advanced features of OO languages by using some common language extensions. We could use \oohaskell to transport the OO design to Haskell were it not that we are bound by UHC's features that does not yet include functional dependencies. The feasibility of the approach therefore hinges on the question whether there exists some other solution which works using UHC and is as powerful as \oohaskell or less powerful but still powerful enough to model the standard features of any OO language. 

{\bf Advantages:}
\begin{itemize}
   \item Will result in a Haskell GUI library that is easier to port to other platforms besides the web platform;
   \item Increased extensibility with the core design writtein in Haskell;
   \item A larger Haskell code base can benefit from compiler optimizations, type safety, etc. 
\end{itemize}

{\bf Disadvantages:}
\begin{itemize}
   \item The non-idiomatic use of Haskell will likely result in a less efficient implementation.
\end{itemize}

\section{Conclusion}

All three options are potentially viable solutions. Option \emph{A} seems to be the least promising approach, because of its many disadvantages compared to the other two options. Option \emph{B} is the most practical option and will most likely be directly useful. However, from a long term perspective it impairs portability of the whole code base across different target platforms/languages and does not benefit much from Haskell as language. Admittedly, this argument is weakened by the fact that the web platform is one of the most widely used standardized platforms available and will without doubt continue to be so in the foreseeable future. However, with the rate at which new GUI toolkits crop up it is hard to say which one will survive, and with a community of Haskell programmers not particulaly fond of \js as a language it is hard to imagine that they would want to maintain a large \js code base. Option C is also from an academic perspective a more interesting path to take, because to the best of our knowledge porting a real-world OO design to Haskell has not been done before. For the above reasons we choose to continue with option C. Next, we will look into interfacing with \js of which the outcomes will also be useful for options A and B.