module InheritanceWithOverride where
import Data.IORef
import Control.Monad.Fix (mfix)
import Prelude hiding ( print )

o # f = f o

data PrintablePointClass a = PrintablePointClass {
    varX :: IORef Int
   ,getX :: IO Int
   ,moveX :: Int -> IO ()
   ,print :: IO ()
   ,printablePointTail :: a
}

printable_point x_init consOp self = do
   x <- newIORef x_init
   cons <- consOp
   return PrintablePointClass {
       varX  = x
      ,getX  = readIORef x
      ,moveX = \d -> modifyIORef x ((+) d)
      ,print = (self # getX) >>= putStr . show
      ,printablePointTail = cons self
   }

data ColoredPointClass a = ColoredPointClass {
    getColor :: IO String
   ,coloredPointTail :: a
}

colored_point' x color consOp self = do
   super <- printable_point x colored_point' self 
   return super {
      print = do putStr "so far - "; super # print
                 putStr "color - "; putStr (show color)
   }

   where

   colored_point' = do
      cons <- consOp
      return $ \self -> ColoredPointClass {
          getColor = return color
         ,coloredPointTail = cons self
      }

getColor' = getColor . printablePointTail

nil = (return :: a -> IO a) $ \_ -> ()

myOverridingOOP = do
   p <- mfix (colored_point' 3 "red" nil)
   p # print

