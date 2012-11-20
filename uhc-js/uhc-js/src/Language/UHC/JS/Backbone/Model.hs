module Language.UHC.JS.Backbone.Model where

import Language.UHC.JS.Primitives
import Language.UHC.JS.Marshal
import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Prelude
import Language.UHC.JS.Types


data BBModelPtr a
type BBModel a = JSObject_ (BBModelPtr a)

foreign import js "Backbone.Model.extend(%*)"
  extend :: JSObject_ a -> IO (JSFunction_ b)

foreign import js "Backbone.Model.extend(%*)"
  extend' :: JSObject_ a -> JSObject_ b -> IO (JSFunction_ c)

get :: BBModel a -> String -> IO b
get p s = _get p (toJS s)

foreign import js "%1.get(%2)"
  _get :: BBModel a -> JSString -> IO b

foreign import js "%1.set(%2)"
  set :: BBModel a -> JSObject_ b -> IO ()

foreign import js "%1.set(%*)"
  set' :: BBModel a -> JSObject_ b -> JSObject_ c -> IO ()

escape :: BBModel a -> String -> IO String
escape p s = liftFromJS_ $ _escape p (toJS s)

foreign import js "%1.escape(%2)"
  _escape :: BBModel a -> JSString -> IO JSString

has :: BBModel a -> String -> IO Bool
has p s = _has p (toJS s)

foreign import js "%1.has(%2)"
  _has :: BBModel a -> JSString -> IO Bool

unset :: BBModel a -> String -> IO ()
unset p s = _unset p (toJS s)

foreign import js "%1.unset(%2)"
  _unset :: BBModel a -> JSString -> IO ()

unset' :: BBModel a -> String -> JSObject_ b -> IO ()
unset' p s o = _unset' p (toJS s) o

foreign import js "%1.unset(%*)"
  _unset' :: BBModel a -> JSString -> JSObject_ b -> IO ()

foreign import js "%1.clear()"
  clear :: BBModel a -> IO ()

foreign import js "%1.clear(%*)"
  clear' :: BBModel a -> JSObject_ b -> IO ()

silentOpt :: IO JSObject
silentOpt = do
  obj <- mkAnonObj
  setAttr "silent" True obj
  return obj

getModelID :: BBModel a -> IO String
getModelID m = liftFromJS_ ida
  where  ida :: IO JSString
         ida = getAttr "id" m

getModelCID :: BBModel a -> IO String
getModelCID m = liftFromJS_ cida
  where  cida :: IO  JSString
         cida = getAttr "cid" m

getModelAttributes :: BBModel a -> IO JSObject
getModelAttributes = getAttr "attributes"

-- TODO: defaults

foreign import js "%1.toJSON()"
  toJSON :: BBModel a -> IO JSObject

foreign import js "%1.fetch()"
  fetch :: BBModel a -> IO ()

foreign import js "%1.fetch(%*)"
  fetch' :: BBModel a -> JSObject_ b -> IO ()

foreign import js "%1.save()"
  save :: BBModel a -> IO ()

foreign import js "%1.save(%*)"
  save' :: BBModel a -> JSObject_ b -> IO ()

foreign import js "%1.save(%*)"
  save'' :: BBModel a -> JSObject_ b -> JSObject_ c -> IO ()

foreign import js "%1.destroy()"
  destroy :: BBModel a -> IO ()

foreign import js "%1.destroy(%*)"
  destroy' :: BBModel a -> JSObject_ b -> IO ()

-- TODO: validate
-- Validate is a method in the Backbone.Model class. It does nothing by default
-- and the user is encouraged to override it. It is passed an object with
-- attributes which need to be validated. If it returns nothing, validation
-- succeeds, otherwise validations has failed. Can we just pass a function like
-- this? Also, will an empty string count as success as well?
setValidateFn :: (JSObject_ a -> JSString) -> BBModel b -> IO (BBModel b)
setValidateFn = setAttr "validate"


getUrl :: BBModel a -> IO String
getUrl = liftFromJS_ . _getUrl

foreign import js "getUrl(%1)"
  _getUrl :: BBModel a -> IO JSString


setUrl :: String -> BBModel a -> IO (BBModel a)
setUrl s m = setAttr "url" s' m
  where  s' :: JSString
         s' = toJS s

setUrl' :: JSFunction_ b -> BBModel a -> IO (BBModel a)
setUrl' = setAttr "url"

getUrlRoot :: BBModel a -> IO String
getUrlRoot m = liftFromJS_ rt
  where  rt :: IO JSString
         rt = getAttr "urlRoot" m

setUrlRoot :: String -> BBModel a -> IO (BBModel a)
setUrlRoot s m = setAttr "urlRoot" s' m
  where  s' :: JSString
         s' = toJS s

-- TODO: Same concerns as setValidateFn
setParseFn :: (JSObject_ a -> JSObject_ a) -> BBModel b -> IO (BBModel b)
setParseFn = setAttr "parse"


foreign import js "%1.clone()"
  clone :: BBModel a -> IO (BBModel a)

foreign import js "%1.isNew()"
  isNew :: BBModel a -> IO Bool

foreign import js "%1.change()"
  change :: BBModel a -> IO ()

foreign import js "%1.hasChanged()"
  hasChanged :: BBModel a -> IO Bool

hasChanged' :: BBModel a -> String -> IO Bool
hasChanged' p a = _hasChanged' p (toJS a)

foreign import js "%1.hasChanged(%2)"
  _hasChanged' :: BBModel a -> JSString -> IO Bool


foreign import js "%1.changedAttributes()"
  changedAttributes :: BBModel a -> IO JSObject

foreign import js "%1.changedAttributes(%2)"
  changedAttributes' :: BBModel a -> JSObject_ b -> IO JSObject

previous :: BBModel a -> String -> IO b
previous p a = _previous p (toJS a)

foreign import js "%1.previous(%2)"
  _previous :: BBModel a -> JSString -> IO b

foreign import js "%1.previousAttributes()"
  previousAttributes :: BBModel a -> IO JSObject
