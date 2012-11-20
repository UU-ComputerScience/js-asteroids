module SelfReference where
import Data.IORef
import Prelude hiding (print)
import Control.Monad.Fix (mfix)

nilRecord = ()
emptyRecord :: IO ()
emptyRecord = return nilRecord

o # f = f o

data PrintablePointClass t = PrintablePointClass {
    getX :: IO Int
   ,moveX :: Int -> IO ()
   ,print :: IO ()
   ,_printablePointTail :: t
}

printable_point x_init cons self = do
   tail <- cons
   varX <- newIORef x_init
   return PrintablePointClass {
       getX  = readIORef varX
      ,moveX = \d -> modifyIORef varX ((+) d)
      ,print = (self # getX) >>= putStr . show
      ,_printablePointTail = tail
   }

mySelfishOOP = do
   p <- mfix $ printable_point 3 emptyRecord
   p # moveX $ 2
   p # print

printable_point' x_init cons self = do
   p <- printable_point x_init cons self
   p # moveX $ 2
   return p

mySelfishOOP' = do
   p <- mfix $ printable_point' 3 emptyRecord
   p # moveX $ 2
   p # print


