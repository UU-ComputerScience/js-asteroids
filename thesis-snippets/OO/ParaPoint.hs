module ParaPoint where
import Data.IORef

data ParaPointClass = ParaPointClass {
    varX      :: IORef Int
   ,getX      :: IO Int
   ,moveX     :: Int -> IO ()
   ,getOffset  :: IO Int
}

para_point x_init = do
   x <- newIORef x_init
   return ParaPointClass {
       varX      = x
      ,getX      = readIORef x
      ,moveX     = \d -> modifyIORef x ((+) d)
      ,getOffset = readIORef x >>= \x -> return (x - x_init)
   }
