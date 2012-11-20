module NestedObject where
import Data.IORef

nilRecord = ()
emptyRecord :: IO ()
emptyRecord = return nilRecord

o # f = f o

data PointClass t = PointClass {
    getX       :: IO Int
   ,moveX      :: Int -> IO ()
   ,_pointTail :: t
}

incrementing_point = do
   x0 <- newIORef 0
   return $ \cons -> do
      tail <- cons
      modifyIORef x0 (+1) 
      varX <- readIORef x0 >>= newIORef 
      return PointClass {
          getX       = readIORef varX
         ,moveX      = \d -> modifyIORef varX (+d)
         ,_pointTail = tail
      }

makeIncrementingPointClass = incrementing_point

myNestedOOP = do
   localClass <- makeIncrementingPointClass
   localClass emptyRecord >>= (# getX) >>= print
   localClass emptyRecord >>= (# getX) >>= print
