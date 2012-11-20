module SimplePoint where

import Data.IORef

data PointClass = PointClass {
    varX      :: IORef Int
   ,getX      :: IO Int
   ,moveX     :: Int -> IO ()
}

o # f = f o

point = do
  x <- newIORef 0
  return PointClass {
     varX      = x
    ,getX      = readIORef x
    ,moveX     = \d -> modifyIORef x ((+) d)
  }

myFirstOOP = do
   p <- point 
   p # getX >>= print
   p # moveX $ 3
   p # getX >>= print
