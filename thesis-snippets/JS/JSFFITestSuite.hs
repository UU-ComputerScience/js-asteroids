module JSFFITestSuite where

import Foreign.Ptr

-- selecting
foreign import js "window" 
   window :: IO ()

foreign import js "window.alert"
   windowAlert :: IO ()

foreign import js "window.%1.style"
   style :: a -> IO ()

foreign import js "%1.%2.style"
   style1 :: a -> b -> IO ()

foreign import js "abc12343.boe"
   testIdent :: IO ()

-- calling
foreign import js "alert()"
   alert :: IO ()

foreign import js "foo(%*)"
   callFoo :: a -> b -> IO ()

-- selecting and calling
foreign import js "window.alert()"
   alert1 :: IO ()

foreign import js "%1.alert(%*)"
   alert2 :: a -> b -> IO ()

foreign import js "{}"
   obj :: IO ()

foreign import js "'Hello World!'"
   helloWorld :: IO a

foreign import js "window.alert(%1)"
   doAlert :: a -> IO ()

main' = helloWorld >>= doAlert
main = return "Hello World!" >>= doAlert

-- new
foreign import js "new Foo()"
   fooObj :: IO ()

foreign import js "new Foo(%*)"
   fooObj1 :: a -> b -> IO ()

foreign import js "new %1.Foo"
   fooObj2 :: a -> IO ()

foreign import js "new %1.%2.Foo(%*)"
   fooObj3 :: a -> b -> c -> IO ()

-- indexing
foreign import js "%1['style']"
   styleIndex :: a -> IO ()

foreign import js "%1['style']['color']"
   styleIndex1 :: a -> IO ()

foreign import js "new %1['style']['color']"
   newColor :: a -> IO ()

foreign import js "[]"
   emptyArr :: IO ()

-- I imagine that this should also be valid...
{-
foreign import js "%1[0]" 
   zeroIndex :: a -> IO ()
-}

-- exports

-- does not work
foreign export js "f" 
   f :: Int -> Int

f x = x * x

foreign import js "dynamic" 
   mkSumFun :: FunPtr (Int -> Int) -> (Int -> Int)

foreign import js "wrapper"
   mkIntCb :: (Int -> IO ()) -> IO (FunPtr (Int -> IO ()))


