{-
interface Document : Node {
  readonly attribute DOMImplementation implementation;
  readonly attribute DOMString URL;
  readonly attribute DOMString documentURI;
  readonly attribute DOMString compatMode;
  readonly attribute DOMString characterSet;
  readonly attribute DOMString contentType;

  readonly attribute DocumentType? doctype;
  readonly attribute Element? documentElement;
  HTMLCollection getElementsByTagName(DOMString localName);
  HTMLCollection getElementsByTagNameNS(DOMString? namespace, DOMString localName);
  HTMLCollection getElementsByClassName(DOMString classNames);
  Element? getElementById(DOMString elementId);

  Element createElement(DOMString localName);
  Element createElementNS(DOMString? namespace, DOMString qualifiedName);
  DocumentFragment createDocumentFragment();
  Text createTextNode(DOMString data);
  Comment createComment(DOMString data);
  ProcessingInstruction createProcessingInstruction(DOMString target, DOMString data);

  Node importNode(Node node, optional boolean deep = true);
  Node adoptNode(Node node);

  Event createEvent(DOMString interface);

  Range createRange();

  // NodeFilter.SHOW_ALL = 0xFFFFFFFF
  NodeIterator createNodeIterator(Node root, optional unsigned long whatToShow = 0xFFFFFFFF, optional NodeFilter? filter = null);
  TreeWalker createTreeWalker(Node root, optional unsigned long whatToShow = 0xFFFFFFFF, optional NodeFilter? filter = null);

  // NEW
  void prepend((Node or DOMString)... nodes);
  void append((Node or DOMString)... nodes);
};

-}
module Language.UHC.JS.HTML5.HTMLDocument where

import Language.UHC.JS.HTML5.Types
import Language.UHC.JS.Types

foreign import js "document"
  document :: IO HTMLDocument

--foreign import js "%1.body"
--  body :: HTMLDocument -> IO (Node ())

--foreign import js "%1.anchors"
--  anchors :: HTMLDocument -> JSArray k Anchor

--foreign import js "%1.forms"
--  forms :: HTMLDocument -> JSArray k Form

--foreign import js "%1.images"
--  images :: HTMLDocument -> JSArray k Image

--foreign import js "%1.links"
--  links :: HTMLDocument -> JSArray k Link

foreign import js "%1.write(%*)"
  write :: HTMLDocument -> JSString -> IO ()

foreign import js "%1.writeln(%*)"
  writeln :: HTMLDocument -> JSString -> IO ()

foreign import js "%1.getElementById(%*)"
  getElementById :: HTMLDocument -> JSString -> IO (Element ())

foreign import js "%1.getElementsByName(%*)"
  getElementsByName :: HTMLDocument -> JSString -> IO HTMLCollection

foreign import js "%1.getElementsByTagName(%*)"
  getElementsByTagName :: HTMLDocument -> JSString -> IO HTMLCollection
  
foreign import js "%1.createElement(%*)"
  createElement :: HTMLDocument -> JSString -> IO (Element ())
