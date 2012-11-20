module InheritanceSimple where
import Data.IORef
import Prelude
import Control.Monad.Fix (mfix)

o # f = f o

data PointClass a = PointClass {
    getX :: IO Int
   ,moveX :: Int -> IO ()
   ,_pointTail :: a
}

point x_init cons self = do
  varX <- newIORef x_init
  tail <- cons
  return PointClass {
     getX = readIORef varX
    ,moveX = \d -> modifyIORef varX (+d)
    ,_pointTail = tail self
  }

data ColoredPointClass a = ColoredPointClass {
    _getColor :: IO String
   ,_coloredPointTail :: a
}

colored_point x color cons self =
   point x colored_point' self 
   where
   colored_point' = do
      tail <- cons
      return $ \self -> ColoredPointClass {
          _getColor = return color
         ,_coloredPointTail = tail self
      }

getColor = _getColor . _pointTail 

nilRecord = ()

emptyRecord = return $ const nilRecord

new :: (IO (a -> ()) -> a -> IO a) -> IO a
new oo = mfix $ oo emptyRecord

myColoredOOP = do
   p <- new $ colored_point 3 "red"
   x <- p # getX
   c <- p # getColor
   print (x, c)

