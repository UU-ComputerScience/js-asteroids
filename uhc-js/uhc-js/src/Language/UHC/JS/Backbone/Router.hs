module Language.UHC.JS.Backbone.Router where

import Language.UHC.JS.Primitives
import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Types
import Language.UHC.JS.Marshal
import Language.UHC.JS.Prelude

data BBRouterPtr
type BBRouter = JSObject_ BBRouterPtr

foreign import js "Backbone.Router.extend(%*)"
  extend :: JSObject_ a -> IO (JSFunction_ b)

foreign import js "Backbone.Router.extend(%*)"
  extend' :: JSObject_ a -> JSObject_ b -> IO (JSFunction_ b)

getRoutes :: BBRouter -> IO JSObject
getRoutes = getAttr "routes"

setRoutes :: JSObject_ a -> BBRouter -> IO BBRouter
setRoutes = setAttr "routes"

route :: BBRouter -> String -> String -> JSFunction_ a -> IO ()
route r s1 s2 f = _route r (toJS s1) (toJS s2) f

foreign import js "%1.route(%*)"
  _route :: BBRouter -> JSString -> JSString -> JSFunction_ a -> IO ()

navigate :: String -> IO ()
navigate = _navigate . toJS

foreign import js "%1.navigate(%*)"
  _navigate :: JSString -> IO ()

navigate' :: String -> Bool -> IO ()
navigate' s b = _navigate' (toJS s) b

foreign import js "%1.navigate(%*)"
  _navigate' :: JSString -> Bool -> IO ()

