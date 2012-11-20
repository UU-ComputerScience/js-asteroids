module GlobalState where

import Data.IORef

foreign import js "x"
   varX :: IO a

foreign import js "mutX()"
   mutX :: IO ()

foreign import js "document.write(%*)"
   documentWrite :: a -> IO ()

data Lens a = Lens (IO a) (a -> IO ())
newtype JSRef t a = JSRef (Lens a)

data Read
data ReadWrite

newReadOnlyJSRef :: IO a -> JSRef Read a
newReadOnlyJSRef r = unsafeCoerce $ newJSRef r undefined

newJSRef :: IO a -> (a -> IO ()) -> JSRef ReadWrite a
newJSRef r w = JSRef (Lens r w)

readJSRef :: JSRef t a -> IO a
readJSRef (JSRef (Lens r _)) = r 

writeJSRef :: JSRef ReadWrite a -> a -> IO ()
writeJSRef (JSRef (Lens _ w)) = w  

foreign import js "window.x"
   readVarX :: IO a

foreign import js "primSetAttr('x',%1,window)"
   writeVarX :: a -> IO ()

jsX = newJSRef readVarX writeVarX
 
main = do
   x <- readJSRef jsX
   documentWrite x
   mutX
   x <- readJSRef jsX
   documentWrite x
   writeJSRef jsX 4
   readJSRef jsX >>= documentWrite