module Language.UHC.JS.Backbone.View where

import Language.UHC.JS.JQuery.JQuery
import Language.UHC.JS.Primitives
import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Types
import Language.UHC.JS.W3C.HTML5
import Language.UHC.JS.Prelude
import Language.UHC.JS.Marshal

data BBViewPtr
type BBView = JSObject_ BBViewPtr

foreign import js "Backbone.View.extend(%*)"
  extend :: JSObject_ a -> IO (JSFunction_ b)

foreign import js "Backbone.View.extend(%*)"
  extend' :: JSObject_ a -> JSObject_ b -> IO (JSFunction_ b)

getEl :: BBView -> IO Element
getEl = getAttr "el"

setEl :: Element -> BBView -> IO BBView
setEl = setAttr "el"

jQuery :: String -> IO JQuery
jQuery = _jQuery . toJS

jQuery' :: String -> JSAny a -> IO JQuery
jQuery' s j = _jQuery' (toJS s) j

setRender :: JSFunction_ a -> BBView -> IO BBView
setRender = setAttr "render"

foreign import js "%1.remove()"
  remove :: BBView -> IO ()

make :: String -> IO Element
make = _make . toJS

foreign import js "%1.make(%*)"
  _make :: JSString -> IO Element

make' :: String -> JSObject_ a -> IO Element
make' s o = _make' (toJS s) o

foreign import js "%1.make(%*)"
  _make' :: JSString -> JSObject_ a -> IO Element

make'' :: String -> JSObject_ a -> String -> IO Element
make'' s1 o s2 = _make'' (toJS s1) o (toJS s2)

foreign import js "%1.make(%*)"
  _make'' :: JSString -> JSObject_ a -> JSString -> IO Element

foreign import js "delegateEvents(%*)"
  delegateEvents :: JSObject_ a -> IO ()
