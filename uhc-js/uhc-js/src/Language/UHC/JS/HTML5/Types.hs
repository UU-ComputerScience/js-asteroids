module Language.UHC.JS.HTML5.Types where

import Language.UHC.JS.Types
import Language.UHC.JS.Prelude

data CNode a
type Node_ a = JSObject_ (CNode a)
type Node = Node_ ()

data Element_ a
type Element a = Node_ (Element_ a)

data CHTMLDocument
type HTMLDocument = JSObject_ CHTMLDocument

data Event_ a 
type Event = JSObject_ (Event_ ())

instance GetObjectRef HTMLElement where
   getObjectRef _ = _htmlElementRef

foreign import js "HTMLElement"
  _htmlElementRef :: b

data CHTMLElement a
type HTMLElement_ a = Element (CHTMLElement a)
type HTMLElement = HTMLElement_ () 

data HTMLImageElement_
type HTMLImageElement = HTMLElement_ HTMLImageElement_

data HTMLCanvasElement_
type HTMLCanvasElement = HTMLElement_ HTMLCanvasElement_

instance GetObjectRef HTMLCanvasElement where
   getObjectRef _ = _htmlCanvasElementRef

foreign import js "HTMLCanvasElement"
  _htmlCanvasElementRef :: b

data AnchorPtr
type Anchor = JSObject_ AnchorPtr

data FormPtr
type Form = JSObject_ FormPtr

data ImagePtr
type Image = JSObject_ ImagePtr

data LinkPtr
type Link  = JSObject_ LinkPtr

data CSSStyleDeclaration_
type CSSStyleDeclaration = JSObject_ CSSStyleDeclaration_

type CSSRule = JSObject
type CSSValue = JSObject

data HTMLCollection_
type HTMLCollection = JSObject_ HTMLCollection_

data NodeList_
type NodeList = JSObject_ NodeList_

data AttrPtr
type Attr    = JSObject_ AttrPtr

data TextPtr
type Text = JSObject_ TextPtr

data Window_ 
type Window = JSObject_ Window_

instance GetObjectRef CanvasRenderingContext2D where
   getObjectRef _ = _canvasRenderingContext2D

foreign import js "CanvasRenderingContext2D"
  _canvasRenderingContext2D :: b

data CanvasRenderingContext2D_
type CanvasRenderingContext2D = JSObject_ CanvasRenderingContext2D_

data NamedNodeMap_
type NamedNodeMap = JSObject_ NamedNodeMap_