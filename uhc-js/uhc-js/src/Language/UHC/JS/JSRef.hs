module Language.UHC.JS.JSRef (
    JSRef
  , newReadOnlyJSRef
  , newJSRef
  , readJSRef
  , writeJSRef
  , ReadWrite
  , Read
) where

import Language.UHC.JS.Types

data Lens a = Lens (IO a) (a -> IO ())
newtype JSRef t a = JSRef (Lens a)

data Read
data ReadWrite

newReadOnlyJSRef :: IO a -> JSRef Read a
newReadOnlyJSRef r = unsafeCoerce $ newJSRef r (error "read only ref")

newJSRef :: IO a -> (a -> IO ()) -> JSRef ReadWrite a
newJSRef r w = JSRef (Lens r w)

readJSRef :: JSRef t a -> IO a
readJSRef (JSRef (Lens r _)) = r 

writeJSRef :: JSRef ReadWrite a -> a -> IO ()
writeJSRef (JSRef (Lens _ w)) = w