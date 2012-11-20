module Language.UHC.JS.HTML5.Window where

import Language.UHC.JS.HTML5.Types
import Language.UHC.JS.Types
import Language.UHC.JS.Marshal
import Language.UHC.JS.Prelude

foreign import js "window"
  window :: IO Window

foreign import js "%1.setInterval(%*)"
  _setInterval :: Window -> JSFunction_ (IO ()) -> Int -> IO Int

foreign import js "%1.alert(%*)"
  _alert :: Window -> JSString -> IO ()

setInterval :: Window -> IO () -> Int -> IO Int
setInterval w f mils = do
  f' <- wrapFunc f 
  _setInterval w f' mils

alert :: Window -> String -> IO ()
alert w = _alert w . toJS

foreign import js "%1.clearInterval(%*)"
  clearInterval :: Window -> Int -> IO ()