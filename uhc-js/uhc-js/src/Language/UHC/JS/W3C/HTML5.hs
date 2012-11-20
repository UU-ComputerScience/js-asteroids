module Language.UHC.JS.W3C.HTML5
  ( Document
  , documentAnchors
  , documentForms
  , documentImages
  , documentLinks
  , document
  , documentWriteln, documentWrite
  , documentGetElementById, documentGetElementsByName, documentGetElementsByTagName
  , documentCreateElement

  , Anchor
  , anchorCharset
  , anchorHref
  , anchorHreflang
  , anchorName
  , anchorRel
  , anchorRev
  , anchorTarget
  , anchorType

  , Form

  , Image

  , Link

  , Element
  , elementValue
  , elementInnerHTML
  , elementTagName
  , elementClientWidth
  , elementClientHeight
  , elementAttributes
  , elementSetAttribute
  , elementAppendChild

  , Attr
  , attrValue

  , NamedNodeMap
  , namedNodeMapLength
  , namedNodeMapItem
  , namedNodeMapNamedItem
  , namedNodeMapRemoveNamedItem
  , namedNodeMapSetNamedItem

  , Node
  , nodeName
  , nodeType

  , NodeList
  , nodeListItem
  , nodeListLength

  , pathName
  , encodeURIComponent
  , _encodeURIComponent
  )
  where

import Language.UHC.JS.Types

import Language.UHC.JS.Primitives
import Language.UHC.JS.ECMA.Array
import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Marshal
import Data.Maybe (fromJust)

data DocumentPtr
type Document = JSObject_ DocumentPtr

foreign import js "document"
  document :: IO Document

foreign import js "%1.anchors"
  documentAnchors :: Document -> JSArray Anchor

foreign import js "%1.forms"
  documentForms :: Document -> JSArray Form

foreign import js "%1.images"
  documentImages :: Document -> JSArray Image

foreign import js "%1.links"
  documentLinks :: Document -> JSArray Link

foreign import js "%1.write(%*)"
  documentWrite :: Document -> JSString -> IO ()

foreign import js "%1.writeln(%*)"
  documentWriteln :: Document -> JSString -> IO ()

foreign import js "%1.getElementById(%*)"
  documentGetElementById :: Document -> JSString -> IO Node

foreign import js "%1.getElementsByName(%*)"
  documentGetElementsByName :: Document -> JSString -> IO (NodeList Node)

documentGetElementsByTagName :: Document -> String -> IO (NodeList Node)
documentGetElementsByTagName d = _documentGetElementsByTagName d . stringToJSString

foreign import js "%1.getElementsByTagName(%*)"
  _documentGetElementsByTagName :: Document -> JSString -> IO (NodeList Node)
  
documentCreateElement :: String -> IO Node
documentCreateElement elem = _documentCreateElement (stringToJSString elem :: JSString)
  
foreign import js "document.createElement(%*)"
  _documentCreateElement :: JSString -> IO Node

data AnchorPtr
type Anchor = JSObject_ AnchorPtr

foreign import js "%1.charset"
  anchorCharset :: Anchor -> JSString

foreign import js "%1.href"
  anchorHref :: Anchor -> JSString

foreign import js "%1.hreflang"
  anchorHreflang :: Anchor -> JSString

foreign import js "%1.name"
  anchorName :: Anchor -> JSString

foreign import js "%1.rel"
  anchorRel :: Anchor -> JSString

foreign import js "%1.rev"
  anchorRev :: Anchor -> JSString

foreign import js "%1.target"
  anchorTarget :: Anchor -> JSString

foreign import js "%1.type"
  anchorType :: Anchor -> JSString

data FormPtr
type Form = JSObject_ FormPtr

foreign import js "%1.elements"
  formElements :: Form -> JSArray Element

data ImagePtr
type Image = JSObject_ ImagePtr

data LinkPtr
type Link  = JSObject_ LinkPtr

data ElementPtr
type Element = JSObject_ ElementPtr

foreign import js "%1.innerHTML"
  elementInnerHTML :: Node -> JSString

foreign import js "%1.value"
  elementValue :: Node -> JSString

foreign import js "%1.tagName"
  elementTagName :: Node -> JSString

foreign import js "%1.clientWidth"
  elementClientWidth :: Node -> Int

foreign import js "%1.clientHeight"
  elementClientHeight :: Node -> Int

foreign import js "%1.attributes"
  elementAttributes :: Node -> NamedNodeMap Node
  
elementSetAttribute :: Node -> String -> String -> IO ()
elementSetAttribute n k v = _elementSetAttribute n (stringToJSString k :: JSString) (stringToJSString v :: JSString)  
  
foreign import js "%1.setAttribute(%*)"
  _elementSetAttribute :: Node -> JSString -> JSString -> IO ()
  
foreign import js "%1.appendChild(%2)"
  elementAppendChild :: Node -> Node -> IO ()

data NodePtr
type Node = JSObject_ NodePtr

foreign import js "%1.nodeName"
  nodeName :: Node -> JSString

foreign import js "%1.nodeType"
  nodeType :: Node -> Int

data NodeListPtr x
type NodeList x = JSObject_ (NodeListPtr x)

foreign import js "%1.length"
  nodeListLength :: NodeList Node -> Int

foreign import js "%1[%2]"
  nodeListItem :: NodeList Node -> Int -> IO Node

data NamedNodeMapPtr x
type NamedNodeMap x    = JSObject_ (NamedNodeMapPtr x)

foreign import js "%1.length"
  namedNodeMapLength :: NamedNodeMap Node -> Int

foreign import js "%1[%2]"
  namedNodeMapItem :: NamedNodeMap Node -> Int -> IO Node

foreign import js "%1.getNamedItem(%*)"
  namedNodeMapNamedItem :: NamedNodeMap Node -> JSString -> IO Node

foreign import js "%1.removeNamedItem(%*)"
  namedNodeMapRemoveNamedItem :: NamedNodeMap Node -> JSString -> IO Node

foreign import js "%1.setNamedItem(%*)"
  namedNodeMapSetNamedItem :: NamedNodeMap Node -> Node -> IO Node

data AttrPtr
type Attr    = JSObject_ AttrPtr

foreign import js "%1.value"
  attrValue :: Attr -> JSString

foreign import js "window.location.pathname"
  _pathName :: IO JSString

pathName :: IO String
pathName = liftFromJS_ _pathName

encodeURIComponent :: String -> String
encodeURIComponent = fromJust . (fromJS :: JSString -> Maybe String) . _encodeURIComponent . (toJS :: String -> JSString)

foreign import js "encodeURIComponent(%1)"
  _encodeURIComponent :: JSString -> JSString
