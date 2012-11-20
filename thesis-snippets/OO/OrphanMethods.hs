{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module OrphanMethods where
import Prelude as Prelude 
import Data.IORef

o # f = f o 

class HasGetX o where
   callGetX :: o -> IO Int

type Foo a = PrintablePointClass (Foo_ a)
data Foo_ a

instance HasGetX (Foo a) where
   callGetX = undefined

instance HasGetX (PrintablePointClass t) where
   callGetX = getX

data PrintablePointClass a = PrintablePointClass {
    varX :: IORef Int
   ,getX :: IO Int
   ,moveX :: Int -> IO ()
   ,print :: IO ()
   ,printablePointTail :: a
}

printable_point x_init cons self = do
   x <- newIORef x_init
   tail <- cons
   return PrintablePointClass {
       varX  = x
      ,getX  = readIORef x
      ,moveX = \d -> modifyIORef x ((+) d)
      ,OrphanMethods.print = print_getX self
      ,printablePointTail = tail self
   }

print_getX self = (self # callGetX) >>= Prelude.print
