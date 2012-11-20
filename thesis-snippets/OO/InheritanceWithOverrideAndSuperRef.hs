module InheritanceWithOverrideAndSuperRef where
import Data.IORef
import Control.Monad.Fix (mfix)
import Prelude hiding ( print )

o # f = f o

nilRecord = ()

emptyRecord = return $ const nilRecord

new :: (IO (a -> ()) -> a -> IO a) -> IO a
new oo = mfix $ oo emptyRecord

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
      ,print = (self # getX) >>= putStr . show
      ,printablePointTail = tail self
   }

data ColoredPointClass a = ColoredPointClass {
    getColor :: IO String
   ,coloredPointTail :: a
}

colored_point' x color cons self = do
   super   <- printable_point x emptyRecord self
   wrapper <- colored_point' super
   return super {
      print = do putStr "so far - "; super # print
                 putStr "color - "; putStr (show color)
      ,printablePointTail = wrapper self
   }

   where

   colored_point' super = do
      tail <- cons
      return $ \self -> ColoredPointClass {
          getColor = do x <- super # getX
                        putStrLn ("Retrieving color at position: " ++ show x) 
                        return color
         ,coloredPointTail = tail self
      }

getColor' = getColor . printablePointTail

myOverridingOOP = do
   p <- new $ colored_point' 3 "red"
   p # getColor'
   p # print

