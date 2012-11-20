{-# LANGUAGE CPP, ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts #-}
{-# OPTIONS -pgmP cpp #-}
module Examples.Shapes where

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
   , _shapeTail  :: Record a
}
DefineClass(Shape,IShape,shapeTail,,1)

#ifdef __UHC__
INSTANCE_TYPEABLE1(IShape,shapeTc,"IShape")
#endif

shape newx newy concreteDraw = clazz $ \tail self -> do
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
       , _shapeTail = tail
    }

data IRectangle a = IRectangle {
    _getWidth  :: IO Int
   ,_getHeight :: IO Int
   ,_setWidth  :: Int -> IO ()
   ,_setHeight :: Int -> IO ()
   ,_rectangleTail :: Record a
}
DefineSubClass(Rectangle,Shape,IRectangle,rectangleTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(IRectangle,rectangleTc,"IRectangle")
#endif

rectangleMethods = unRecord . get_Shape_Tail
getWidth  = _getWidth  . rectangleMethods
getHeight = _getHeight . rectangleMethods
setWidth  = _getWidth  . rectangleMethods
setHeight = _getHeight . rectangleMethods

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
         , _rectangleTail  = tail
      }

   draw self = printLn ("Drawing a Rectangle at:(" <<
                  self # getX << ", " << self # getY <<
                  "), width "  << self # getWidth << 
                  ", height " << self # getHeight)

data ICircle a = ICircle {
     _getRadius :: IO Int
   , _setRadius :: Int -> IO ()
   , _circleTail :: Record a
}

DefineSubClass(Circle,Shape,ICircle,circleTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(ICircle,circleTc,"ICircle")
#endif

circleMethods = unRecord . get_Shape_Tail
getRadius = _getRadius . circleMethods
setRadius = _setRadius . circleMethods

circle x y radius = 
   (wrapper `extends` shape x y draw) noOverride set_Shape_Tail
   where
   wrapper tail super self = do
      radiusRef <- newIORef radius
      return ICircle {
           _getRadius = readIORef radiusRef
         , _setRadius = writeIORef radiusRef
         , _circleTail = tail
      }

   draw self = printLn ("Drawing a Circle at:(" 
                     << self # getX << "," << self # getY
                     << "), radius " << self # getRadius)

myOOP = do
   s1 <- new $ rectangle 10 20 5 6
   s2 <- new $ circle 15 25 8

   let scribble :: [Shape]
       scribble = consUb s1 (consUb s2 nilUb)
  
   {-
   let scribble' :: [Rectangle]
       scribble' = foldr consLb nilLb scribble 

   let scribble' :: [Rectangle]
       scribble' = fdown scribble 

   sequence_ $
      map (# draw) scribble'

   -}
   sequence_ $ 
      map (\shape -> do 
            shape # draw
            (shape # rMoveTo) 100 100
            shape # draw
          ) scribble   
    
   return ()

selectiveDraw shape = do
  let shape' = upcast shape :: Shape
  if shape' `instanceof` (undefined :: Rectangle)
   then shape' # draw
   else return ()

trySelectiveDraw = do
  s1 <- new (rectangle 10 20 5 6)
  s2 <- new (circle 15 25 8)
  s1 # selectiveDraw
  s2 # selectiveDraw

main = myOOP

data ShapeExt = forall a. ShapeExt { unShape :: IShape a }

quantify :: IShape () -> ShapeExt
quantify s = ShapeExt s

draw' (ShapeExt s) = s # draw

car = do
  s <- new $ shape 10 20 (\_ -> putStrLn "42")
  draw' (quantify s)