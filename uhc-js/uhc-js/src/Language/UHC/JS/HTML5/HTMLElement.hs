module Language.UHC.JS.HTML5.HTMLElement where

import Language.UHC.JS.Types
import Language.UHC.JS.Marshal
import Language.UHC.JS.Primitives
import Language.UHC.JS.HTML5.Types
import Language.UHC.JS.Prelude
import Language.UHC.JS.JSRef

onkeydown :: JSObject_ a -> JSRef ReadWrite (Event -> IO ())
onkeydown e = 
  let g = do
         f <- getAttr "onkeydown" e
         if _primIsNull f 
            then return $ const (return ())
            else unwrapFunc1 (unsafeCoerce f)
      s f = wrapFunc1 f >>= \f -> setAttr_ "onkeydown" f e
  in newJSRef g s