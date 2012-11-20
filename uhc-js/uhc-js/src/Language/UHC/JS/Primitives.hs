module Language.UHC.JS.Primitives where

import Language.UHC.JS.Types

foreign import prim "primMkAnonObj"
  mkAnonObj :: IO JSObject

foreign import prim "primEq"
   _primEq :: JSAny a -> JSAny b -> Bool

foreign import prim "primMkObj"
  _primNewObj :: JSString -> IO JSObject

foreign import prim "primMkCtor"
  _primMkCtor :: JSString -> IO (JSFunction_ a)

foreign import prim "primGetCtor"
  _primGetCtor :: JSString -> IO (JSFunction_ a)

foreign import prim "primSetCtor"
  _primSetCtor :: JSString -> JSFunction_ a -> IO ()

foreign import prim "primGetAttr"
  _primGetAttr :: JSString -> JSObject_ p -> IO a

foreign import prim "primPureGetAttr"
  _primPureGetAttr :: JSString -> JSObject_ p -> a

foreign import prim "primSetAttr"
  _primSetAttr :: JSString -> a -> JSObject_ p -> IO (JSObject_ p)

foreign import prim "primPureSetAttr"
  _primPureSetAttr :: JSString -> a -> JSObject_ p -> JSObject_ p

foreign import prim "primModAttr"
  _primModAttr :: JSString -> (a -> b) -> JSObject_ p -> IO (JSObject_ p)

foreign import prim "primPureModAttr"
  _primPureModAttr :: JSString -> (a -> b) -> JSObject_ p -> JSObject_ p

foreign import prim "primGetProtoAttr"
  _primGetProtoAttr :: JSString -> JSString -> IO a

foreign import prim "primSetProtoAttr"
  _primSetProtoAttr :: JSString -> a -> JSString -> IO ()

foreign import prim "primModProtoAttr"
  _primModProtoAttr :: JSString -> (a -> b) -> JSString -> IO ()

foreign import prim "primClone"
  _primClone :: JSObject_ a -> JSObject_ a

foreign import prim "primToPlainObj"
  _primToPlainObj :: JSObject_ a -> JSObject_ b

foreign import prim "primInstanceOf"
  _primInstanceOf :: a -> b -> Bool

foreign import prim "primIsNull"
  _primIsNull :: a -> Bool

foreign import prim "primIsUndefined"
  _primIsUndefined :: a -> Bool

foreign import prim "primIsBool"
  _primIsBool :: a -> Bool

foreign import prim "primIsString"
  _primIsString :: a -> Bool

foreign import prim "primIsChar"
  _primIsChar :: a -> Bool

foreign import prim "primIsNumber"
  _primIsNumber :: a -> Bool

foreign import prim "primIsDouble"
  _primIsDouble :: a -> Bool

foreign import prim "primIsObject"
  _primIsObject :: a -> Bool

foreign import prim "primIsFunction"
  _primIsFunction :: a -> Bool

foreign import prim
  primNewArray :: Int -> x -> JSArray x

foreign import prim "primWriteArray"
  primWriteArray :: JSArray x -> Int -> x -> ()

foreign import prim "primStrictWriteArray"
  primStrictWriteArray :: JSArray x -> Int -> x -> ()