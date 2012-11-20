module Language.UHC.JS.WebWorker where

import Language.UHC.JS.Prelude

data WebWorkerPtr
type WebWorker = JSPtr WebWorkerPtr  

newWorker :: String -> IO WebWorker
newWorker = _newWorker . toJS

foreign import js "newWorker(%1)"
  _newWorker :: JSString -> IO WebWorker

setOnMessage :: WebWorker -> (JSPtr a -> IO ()) -> IO ()
setOnMessage self f = do 
  f'   <- wrapJSPtraIO f
  setAttr "onmessage" f' self
  return ()
  
foreign import js "JSON.stringify(%1)"
  jsonStringify :: a -> JSString

foreign import js "JSON.parse(%1)"
  jsonParse :: JSString -> IO a

postMessage :: WebWorker -> a -> IO ()
postMessage =  _postMessage 
  
foreign import js "%1.postMessage(%2)"
  _postMessage :: WebWorker -> a -> IO ()
  
foreign import js "self"
  getSelf :: IO WebWorker