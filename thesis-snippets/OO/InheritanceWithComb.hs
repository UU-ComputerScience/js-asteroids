{-# LANGUAGE MultiParamTypeClasses #-}
module InheritanceWithComb where
import Data.IORef
import Control.Monad.Fix (mfix)
import Prelude hiding ( print )

o # f = f o

nilRecord = ()

emptyRecord = return $ const nilRecord

new :: (IO (a -> ()) -> a -> IO a) -> IO a
new oo = mfix $ oo emptyRecord

inherit ::
        (cons -> super -> IO (self -> w))
     -> (IO (a -> ()) -> self -> IO super)
     -> (super -> IO (self -> super'))
     -> (super' -> w -> b)
     -> cons
     -> self
     -> IO b
inherit w g override oplus = \cons self -> do
   super   <- g emptyRecord self
   wrapper <- w cons super
   super'  <- override super 
   return $ (super' self) `oplus` (wrapper self)

b `extends` a = inherit b a

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

colored_point x color = 
   (wrapper `extends` printable_point x) override (\o v -> o { printablePointTail = v })

   where

   override super = return $ \self -> super {
      print = do putStr "so far - "; super # print
                 putStr "color - "; putStr (show color)
   }

   wrapper cons super = do
      tail <- cons
      return $ \self -> ColoredPointClass {
          getColor = do x <- super # getX
                        putStrLn ("Retrieving color at position: " ++ show x) 
                        return color
         ,coloredPointTail = tail self
      }

getColor' = getColor . printablePointTail

colored_point' x_init color cons self = do 
   super <- colored_point x_init color cons self
   return $ super { 
      print = putStr "I'm a colored point"
   }  

myOverridingOOP = do
   p <- new $ colored_point 3 "red"
   p # getColor'
   p # print

