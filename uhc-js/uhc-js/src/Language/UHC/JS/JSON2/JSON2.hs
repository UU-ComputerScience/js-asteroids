module Language.UHC.JS.JSON2.JSON2 where

-- | Wrapper for json2.js, as found at http://documentcloud.github.com/backbone

import Language.UHC.JS.ECMA.Array
import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Primitives
import Language.UHC.JS.Types
import Language.UHC.JS.Marshal


data JSONPtr
type JSON = JSObject_ JSONPtr

stringify :: JSAny a -> IO String
stringify = liftFromJS_ . _stringify

foreign import js "JSON.stringify(%*)"
  _stringify :: JSAny a -> IO JSString

stringify' :: JSArray a -> IO String
stringify' = liftFromJS_ . _stringify'

foreign import js "JSON.stringify(%*)"
  _stringify' :: JSArray a -> IO JSString

-- TODO: All permutations for stringify

parse :: String -> IO (JSAny a)
parse = _parse . toJS

parse' :: String -> IO () -> IO (JSAny a)
parse' s c = _parse' (toJS s) c

foreign import js "JSON.parse(%*)"
  _parse :: JSString -> IO (JSAny a)

foreign import js "JSON.parse(%*)"
  _parse' :: JSString -> IO () -> IO (JSAny a)
