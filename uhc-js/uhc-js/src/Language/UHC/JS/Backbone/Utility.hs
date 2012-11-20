module Language.UHC.JS.Backbone.Utility where

import Language.UHC.JS.Primitives
import Language.UHC.JS.Types

data BackbonePtr
type Backbone = JSObject_ BackbonePtr

foreign import js "Backbone.noConflict()"
  noConflict :: Backbone
