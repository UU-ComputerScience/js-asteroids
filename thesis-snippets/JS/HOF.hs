module HOF where

data JSObject a

data JSBool_
type JSBool = JSObject JSBool_

data JSNumber_
type JSNumber = JSObject JSNumber_

data JSString_
type JSString = JSObject PackedString

data JSFunction_ a
type JSFunction a = JSObject (JSFunction_ a)

foreign import js "twice(%*)"
  _twice :: JSFunction (IO ()) -> IO ()

foreign import js "wrapper"
  _twice_hof :: IO () -> JSFunction (IO ())

twice :: IO () -> IO ()
twice = _twice . _twice_hof


hof1 :: (JSBool -> IO JSString) -> IO JSString
hof1 = _hof1 . _hof1_f

foreign import js "hof1(%*)"
   _hof1 :: JSFunction (JSBool -> IO JSString) -> IO JSString

foreign import js "wrapper"
  _hof1_f :: (JSBool -> IO JSString) -> JSFunction (JSBool -> IO JSString)

foreign import js "console.log(%1)"
   log :: a -> IO ()

foreign import js "'foo'"
   _foo_str :: JSString

foreign import js "createCounter()"
   createCounter :: IO (JSFunction (IO Int))

foreign import js "dynamic"
   mkFun :: JSFunction (IO Int) -> IO Int

main = do
   twice (putStr "Hello World!")

   r <- hof1 (\b -> log b >> return _foo_str) 
   log r

   jsF <- createCounter
   let f = mkFun jsF
   
   mapM_ (\m -> m >>= print) [f,f,f]
