module Language.UHC.JS.Backbone.Collection where

import Language.UHC.JS.Backbone.Model
import Language.UHC.JS.ECMA.Array
import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Primitives
import Language.UHC.JS.Types
import Language.UHC.JS.Marshal
import Language.UHC.JS.Prelude

data BBCollectionPtr a
type BBCollection a = JSObject_ (BBCollectionPtr a)

foreign import js "Backbone.Collection.extend(%*)"
  extend :: JSObject_ a -> IO (JSFunction_ b)

foreign import js "Backbone.Collection.extend(%*)"
  extend' :: JSObject_ a -> JSObject_ b -> IO (JSFunction_ b)

model :: JSFunction_ b -> BBCollection a -> IO (BBCollection a)
model = setAttr "model"

models :: BBCollection a -> IO (JSArray (BBModel b))
models = getAttr "models"

foreign import js "%1.toJSON()"
  toJSON :: BBCollection a -> IO (JSArray b)

-- TODO: Underscore methods

foreign import js "%1.add(%*)"
  add :: BBCollection a -> BBModel b -> IO ()

foreign import js "%1.add(%*)"
  add' :: BBCollection a -> BBModel b -> JSObject_ c -> IO ()

foreign import js "%1.add(%*)"
  addA :: BBCollection a -> JSArray (BBModel b) -> IO ()

foreign import js "%1.add(%*)"
  addA' :: BBCollection a -> JSArray (BBModel b) -> JSObject_ c -> IO ()

foreign import js "%1.remove(%*)"
  remove :: BBCollection a -> BBModel b -> IO ()

foreign import js "%1.remove(%*)"
  remove' :: BBCollection a -> BBModel b -> JSObject_ c -> IO ()

foreign import js "%1.remove(%*)"
  removeA :: BBCollection a -> JSArray (BBModel b) -> IO ()

foreign import js "%1.remove(%*)"
  removeA' :: BBCollection a -> JSArray (BBModel b) -> JSObject_ c -> IO ()

foreign import js "%1.get(%*)"
  get :: BBCollection a -> Int -> IO (BBModel b)


getByCid :: String -> BBCollection a -> IO (BBModel b)
getByCid s c = _getByCid (toJS s) c

foreign import js "%1.getByCid(%*)"
  _getByCid :: JSString -> BBCollection a -> IO (BBModel b)


foreign import js "%1.at(%*)"
  at :: Int -> IO (BBModel a)

clength :: BBCollection a -> IO Int
clength = getAttr "length"

setComperator :: JSFunction_ a -> BBCollection b -> IO (BBCollection b)
setComperator = setAttr "comparator"


foreign import js "%1.sort()"
  sort :: BBCollection a -> IO ()

foreign import js "%1.sort(%*)"
  sort' :: BBCollection a -> JSFunction_ b -> IO ()

pluck :: BBCollection a -> String -> IO (JSArray b)
pluck c s = _pluck c (toJS s)

foreign import js "%1.pluck(%*)"
  _pluck :: BBCollection a -> JSString -> IO (JSArray b)

setUrl :: String -> BBCollection a -> IO (BBCollection a)
setUrl s m = setAttr "url" s' m
  where  s' :: JSString
         s' = toJS s

setUrl' :: JSFunction_ b -> BBCollection a -> IO (BBCollection a)
setUrl' = setAttr "url"

-- TODO: parse

foreign import js "%1.fetch()"
  fetch :: BBCollection a -> IO ()

foreign import js "%1.fetch(%*)"
  fetch' :: BBCollection a -> JSFunction_ b -> IO ()

foreign import js "%1.reset(%*)"
  reset :: BBCollection a -> JSArray (BBModel b) -> IO ()

foreign import js "%1.reset(%*)"
  reset' :: BBCollection a -> JSArray (BBModel b) -> JSFunction_ c -> IO ()

foreign import js "%1.create(%*)"
  create :: BBCollection a -> JSArray (BBModel b) -> IO ()

foreign import js "%1.create(%*)"
  create' :: BBCollection a -> JSArray (BBModel b) -> JSFunction_ c -> IO ()
