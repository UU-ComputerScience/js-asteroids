module Language.UHC.JS.Backbone.Sync where

import Language.UHC.JS.Backbone.Model
import Language.UHC.JS.Primitives
import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Types
import Language.UHC.JS.Marshal

sync :: String -> BBModel a -> IO ()
sync s = _sync (toJS s)

foreign import js "Backbone.sync(%*)"
  _sync :: JSString -> BBModel a -> IO ()

sync' :: String -> BBModel a -> JSObject_ b -> IO ()
sync' s = _sync' (toJS s)

foreign import js "Backbone.sync(%*)"
  _sync' :: JSString -> BBModel a -> JSObject_ b -> IO ()

-- TODO: emulateHTTP
-- TODO: emulateJSON
