module SimplePoinTyExt where

import Data.IORef

data PointClass a = PointClass {
    varX      :: IORef Int
   ,getX      :: IO Int
   ,moveX     :: Int -> IO ()
   ,pointTail :: a
}

data Point2DClass a = Point2DClass {
    _getY        :: IO Int
   ,_point2DTail :: a
} 

o # f = f o

nilRecord = ()
emptyRecord :: IO ()
emptyRecord = return nilRecord

point cons = do
  x <- newIORef 0
  tail <- cons
  return PointClass {
     varX      = x
    ,getX      = readIORef x
    ,moveX     = \d -> modifyIORef x ((+) d)
    ,pointTail = tail
  }

point2d cons = do
   point point2d'
   where
   point2d' = do
      y <- newIORef 0
      tail <- cons
      return Point2DClass {
          _getY = readIORef y
         ,_point2DTail = tail
      }

getY = _getY . pointTail

myFirstOOP = do
   p <- point emptyRecord 
   p # getX >>= print
   p # moveX $ 3
   p # getX >>= print
