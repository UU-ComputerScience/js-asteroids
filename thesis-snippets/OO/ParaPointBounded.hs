{-# LANGUAGE ExistentialQuantification,DatatypeContexts #-}
module ParaPointBounded where
import Data.IORef
{-
data Num a => ParaPointClass a = ParaPointClass {
   varX      :: IORef a
   ,getX      :: IO a
   ,moveX     :: a -> IO ()
   ,getOffset  :: IO a
}
-}
{-
data ParaPointClass = forall a. Num a => ParaPointClass {
    varX      :: IORef a
   ,getX      :: IO a
   ,moveX     :: a -> IO ()
   ,getOffset  :: IO a
}
-}
o # f = f o

data ParaPointClass a = ParaPointClass {
   varX      :: IORef a
   ,getX      :: IO a
   ,moveX     :: a -> IO ()
   ,getOffset  :: IO a
}

para_point x_init = do
   x <- newIORef x_init
   return ParaPointClass {
       varX      = x
      ,getX      = readIORef x
      ,moveX     = \d -> modifyIORef x ((+) d)
      ,getOffset = readIORef x >>= \x -> return (x - x_init)
   }

test = do
   p1 <- para_point (3 :: Int)
   p2 <- para_point (3.0 :: Double)
   o <- getOffset p2
   moveX p1 $ 2.0
   putStrLn (show o)
   return ()

myPolyOOP = do
   p <- para_point (1::Int)
   p' <- para_point (1::Double)
   p # moveX $ 2
   p' # moveX $ 2.5
   p # getX >>= print
   p' # getX >>= print

