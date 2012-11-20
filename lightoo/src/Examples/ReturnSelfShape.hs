{-# LANGUAGE CPP, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts #-}
{-# OPTIONS -pgmP cpp #-}
module Examples.ReturnSelfShape where

#ifdef __UHC__
#include "../LightOOUHC.h"
#include "Typeable.h"
#else
#include "../LightOO.h"
#endif

import LightOO
import Examples.Print
import Data.Typeable
import Data.IORef

data IShape a = IShape {
     getX       :: IO Int
   , getY       :: IO Int
   , setX       :: Int -> IO ()
   , setY       :: Int -> IO ()
   , moveTo     :: Int -> Int -> IO ()
   , rMoveTo    :: Int -> Int -> IO ()
   , draw       :: IO ()
   , meShape         :: IO Shape
   , _shapeTail  :: Record a
}
DefineClass(Shape,IShape,shapeTail,,1)

#ifdef __UHC__
INSTANCE_TYPEABLE1(IShape,shapeTc,"Shape")
#endif

shape newx newy concreteDraw = 
   clazz $ \tail self -> do
      x <- newIORef newx
      y <- newIORef newy
      return IShape {
           getX = readIORef x 
         , getY = readIORef y
         , setX = writeIORef x
         , setY = writeIORef y
         , moveTo = \newx newy -> do
            self # setX $ newx
            self # setY $ newy
         , rMoveTo = \deltax deltay -> do
            x <- self # getX
            y <- self # getY
            (self # moveTo) (x + deltax) (y + deltay)
         , draw = concreteDraw self
         , meShape = return (upcast self :: Shape)
         , _shapeTail = tail
      }

data IRectangle a = IRectangle {
    _getWidth  :: IO Int
   ,_getHeight :: IO Int
   ,_setWidth  :: Int -> IO ()
   ,_setHeight :: Int -> IO ()
   ,_meRect :: IO Rectangle
   ,_rectangleTail :: Record a
}
DefineSubClass(Rectangle,Shape,IRectangle,rectangleTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(IRectangle,rectangleTc,"Rectangle")
#endif

rectangleMethods = unRecord . get_Shape_Tail
getWidth  = _getWidth  . rectangleMethods
getHeight = _getHeight . rectangleMethods
setWidth  = _getWidth  . rectangleMethods
setHeight = _getHeight . rectangleMethods
meRect    = _meRect    . rectangleMethods

rectangle x y width height =
   (wrapper `extends` shape x y draw) noOverride set_Shape_Tail
   where
   wrapper tail super self = do
      w <- newIORef width
      h <- newIORef height
      return IRectangle {
           _getWidth      = readIORef w
         , _getHeight     = readIORef h
         , _setWidth      = writeIORef w 
         , _setHeight     = writeIORef h
         , _meRect         = return $ upcast self
         , _rectangleTail  = tail
      }

   draw self = printLn ("Drawing a Rectangle at:(" <<
                  self # getX << ", " << self # getY <<
                  "), width "  << self # getWidth << 
                  ", height " << self # getHeight)

mySelf = do
   s1 <- new $ rectangle 10 20 5 6

   shape <- s1 # meShape
   shape # draw
   let Just rect = downcast shape :: Maybe Rectangle
   rect # getWidth >>= putStrLn . show  
   
   return ()

