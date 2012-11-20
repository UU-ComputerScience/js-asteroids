module Language.UHC.JS.Backbone.History where

import Language.UHC.JS.Primitives
import Language.UHC.JS.Types

foreign import js "Backbone.history.start()"
  start :: IO ()

foreign import js "Backbone.history.start(%*)"
  start' :: JSObject_ a -> IO ()
