{-# LANGUAGE CPP, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, ExistentialQuantification #-}
{-# OPTIONS -pgmP cpp #-}
module Examples.ReturnSelfShapeExt where

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
   , meShape     :: ShapeExt
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
         , meShape = ShapeExt $ return self
         , _shapeTail = tail
      }

data ShapeExt = forall t. (Sub (Shape_ t) Shape) => ShapeExt (IO (Shape_ t))

data IRectangle a = IRectangle {
    _getWidth  :: IO Int
   ,_getHeight :: IO Int
   ,_setWidth  :: Int -> IO ()
   ,_setHeight :: Int -> IO ()
   ,_meRect    :: RectExt
   ,_rectangleTail :: Record a
}
DefineSubClass(Rectangle,Shape,IRectangle,rectangleTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(IRectangle,rectangleTc,"Rectangle")
#endif

data RectExt = forall t. (Sub (Rectangle_ t) Rectangle) => RectExt (IO (Rectangle_ t))

rectangleMethods = unRecord . get_Shape_Tail
getWidth  = _getWidth  . rectangleMethods
getHeight = _getHeight . rectangleMethods
setWidth  = _getWidth  . rectangleMethods
setHeight = _getHeight . rectangleMethods
meRect = _meRect . rectangleMethods

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
         , _meRect        = RectExt $ return self
         , _rectangleTail  = tail
      }

   draw self = printLn ("Drawing a Rectangle at:(" <<
                  self # getX << ", " << self # getY <<
                  "), width "  << self # getWidth << 
                  ", height " << self # getHeight)

mySelf = do
   s1 <- new $ rectangle 10 20 5 6
   let m = s1 # meShape
   case m of
      ShapeExt m -> do s1 <- m
                       s1 # draw
                       let shape = upcast s1 :: Shape
                       let Just s1 = downcast shape :: Maybe Rectangle
                       s1 # getWidth >>= putStrLn . show


