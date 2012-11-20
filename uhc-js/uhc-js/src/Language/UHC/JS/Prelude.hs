module Language.UHC.JS.Prelude where

import Language.UHC.JS.Types
import Language.UHC.JS.Marshal
import Language.UHC.JS.Primitives   
import Language.UHC.JS.ECMA.String  

class GetObjectRef a where
  getObjectRef :: a -> b

cast :: GetObjectRef b => a -> Maybe b
cast a :: Maybe b =
  if _primInstanceOf a (getObjectRef (undefined :: b))
   then Just (unsafeCoerce a)
   else Nothing

foreign import js "wrapper"
  wrapFunc :: IO a -> IO (JSFunction_ (IO a))

foreign import js "wrapper"
  wrapFunc1 :: (a -> IO b) -> IO (JSFunction_ (a -> IO b))

foreign import js "dynamic"
   unwrapFunc :: JSFunction_ (IO a) -> IO (IO a)

foreign import js "dynamic"
   unwrapFunc1 :: JSFunction_ (a -> IO b) -> IO (a -> IO b)

newObj :: String -> IO JSObject
newObj = _primNewObj . toJS

mkCtor :: String -> IO (JSFunction_ a)
mkCtor = _primMkCtor . toJS

getCtor :: String -> IO (JSFunction_ a)
getCtor s1 = _primGetCtor (toJS s1)

setCtor :: String -> JSFunction_ a -> IO ()
setCtor s1 fp = _primSetCtor (toJS s1) fp

getAttr :: String -> JSObject_ p -> IO a
getAttr s p = _primGetAttr (toJS s) p

setAttr :: String -> a -> JSObject_ p -> IO (JSObject_ p)
setAttr s a p = _primSetAttr (toJS s) a p

setAttr_ :: String -> a -> JSObject_ p -> IO ()
setAttr_ s a p = setAttr s a p >> return ()

pureSetAttr :: String -> a -> JSObject_ p -> JSObject_ p
pureSetAttr s a p = _primPureSetAttr (toJS s) a p

modAttr :: String -> (a -> b) -> JSObject_ p -> IO (JSObject_ p)
modAttr s f p = _primModAttr (toJS s) f p

pureModAttr :: String -> (a -> b) -> JSObject_ p -> JSObject_ p
pureModAttr s f p = _primPureModAttr (toJS s) f p

getProtoAttr :: String -> String -> IO a
getProtoAttr x y = _primGetProtoAttr (toJS x) (toJS y)

setProtoAttr :: String -> a -> String -> IO ()
setProtoAttr x a y = _primSetProtoAttr (toJS x) a (toJS y)

modProtoAttr :: String -> (a -> b) -> String -> IO ()
modProtoAttr x f y = _primModProtoAttr (toJS x) f (toJS y)

foreign import js "{}"
  mkObj :: a -> IO (JSObject_ b)

foreign import js "console.log(%*)"
  _trace :: a -> IO ()
