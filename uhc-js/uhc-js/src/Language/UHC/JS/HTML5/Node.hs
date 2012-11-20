{-
interface Node : EventTarget {
  const unsigned short ELEMENT_NODE = 1;
  const unsigned short ATTRIBUTE_NODE = 2; // historical
  const unsigned short TEXT_NODE = 3;
  const unsigned short CDATA_SECTION_NODE = 4; // historical
  const unsigned short ENTITY_REFERENCE_NODE = 5; // historical
  const unsigned short ENTITY_NODE = 6; // historical
  const unsigned short PROCESSING_INSTRUCTION_NODE = 7;
  const unsigned short COMMENT_NODE = 8;
  const unsigned short DOCUMENT_NODE = 9;
  const unsigned short DOCUMENT_TYPE_NODE = 10;
  const unsigned short DOCUMENT_FRAGMENT_NODE = 11;
  const unsigned short NOTATION_NODE = 12; // historical
  readonly attribute unsigned short nodeType;
  readonly attribute DOMString nodeName;

  readonly attribute DOMString? baseURI;

  readonly attribute Document? ownerDocument;
  readonly attribute Node? parentNode;
  readonly attribute Element? parentElement;
  boolean hasChildNodes();
  readonly attribute NodeList childNodes;
  readonly attribute Node? firstChild;
  readonly attribute Node? lastChild;
  readonly attribute Node? previousSibling;
  readonly attribute Node? nextSibling;

  const unsigned short DOCUMENT_POSITION_DISCONNECTED = 0x01;
  const unsigned short DOCUMENT_POSITION_PRECEDING = 0x02;
  const unsigned short DOCUMENT_POSITION_FOLLOWING = 0x04;
  const unsigned short DOCUMENT_POSITION_CONTAINS = 0x08;
  const unsigned short DOCUMENT_POSITION_CONTAINED_BY = 0x10;
  const unsigned short DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = 0x20; // historical
  unsigned short compareDocumentPosition(Node other);
  boolean contains(Node? other);

           attribute DOMString? nodeValue;
           attribute DOMString? textContent;
  Node insertBefore(Node node, Node? child);
  Node_ appendChild(Node node);
  Node replaceChild(Node node, Node child);
  Node removeChild(Node child);
  void normalize();

  
  Node cloneNode(optional boolean deep = true);
  boolean isEqualNode(Node? node);

  DOMString lookupPrefix(DOMString? namespace);
  DOMString lookupNamespaceURI(DOMString? prefix);
  boolean isDefaultNamespace(DOMString? namespace);
};
-}
module Language.UHC.JS.HTML5.Node where

import Language.UHC.JS.HTML5.Types
import Language.UHC.JS.Types

foreign import js "%1.nodeType"
   nodeType :: Node_ a -> IO Int

foreign import js "%1.ownerDocument"
   ownerDocument :: Node_ a -> IO HTMLDocument

foreign import js "%1.parentNode"
   parentNode :: Node_ a -> IO Node

foreign import js "%1.parentElement"
   parentElement :: Node_ a -> IO Element

foreign import js "%1.hasChildNodes()"
   hasChildNodes :: Node_ a -> IO JSBool

foreign import js "%1.firstChild"
   firstChild :: Node_ a -> IO Node

foreign import js "%1.lastChild"
   lastChild :: Node_ a -> IO Node

foreign import js "%1.previousSibling"
   previousSibling :: Node_ a -> IO Node

foreign import js "%1.nextSibling"
   nextSibling :: Node_ a -> IO Node

foreign import js "%1.contains(%*)"
   contains :: Node_ a -> IO JSBool

foreign import js "%1.nodeValue"
   nodeValue :: Node_ a -> IO JSString

foreign import js "%1.textContent"
   textContent :: Node_ a -> IO JSString

foreign import js "%1.insertBefore(%*)"
   insertBefore :: Node_ a -> Node_ b -> Node_ c -> IO Node

foreign import js "%1.appendChild(%*)"
   appendChild :: Node_ a -> Node_ b -> IO Node

foreign import js "%1.replaceChild(%*)"
   replaceChild :: Node_ a -> Node_ n -> Node_ c -> IO Node

foreign import js "%1.removeChild(%*)"
   removeChild :: Node_ a -> Node b -> IO Node