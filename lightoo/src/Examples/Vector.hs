{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, Rank2Types, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS -pgmP cpp #-}
module Examples.Vector where

#ifdef __UHC__
#include "../LightOOUHC.h"
#include "Typeable.h"
#else
#include "../LightOO.h"
#endif

import Prelude as P
import LightOO
import Data.IORef
import Data.Typeable

data IPrintablePoint t = IPrintablePoint {
    _getX :: IO Int
   ,_moveX :: Int -> IO ()
   ,_print :: IO ()
   ,_printablePointTail :: Record t
}
DefineSubClass(PrintablePoint,Object,IPrintablePoint,printablePointTail,,,,1,)

printable_point x_init = do
   (wrapper `extends` object) noOverride set_Object_Tail
   where
   wrapper tail super self = do
      x <- newIORef x_init
      return IPrintablePoint {
          _getX = readIORef x
         ,_moveX = \d -> modifyIORef x ((+) d)
         ,_print = self # getX >>= P.print
         ,_printablePointTail = tail
      }
   
getX = _getX . unRecord . get_Object_Tail
pointPrint = _print . unRecord . get_Object_Tail
moveX = _moveX . unRecord . get_Object_Tail

data IColoredPoint t = IColoredPoint {
    _coloredPointGetColor :: IO String
   ,_coloredPointTail :: Record t
}
DefineSubClass(ColoredPoint,PrintablePoint,IColoredPoint,coloredPointTail,,,,1,)

coloredPointGetColor = _coloredPointGetColor . unRecord . get_PrintablePoint_Tail

colored_point x_init (color :: String) =
   (wrapper `extends` printable_point x_init) noOverride set_PrintablePoint_Tail
   where
   wrapper tail super self = do
      return IColoredPoint {
          _coloredPointGetColor = return color
         ,_coloredPointTail = tail
      }

-- OOHaskell 5.9 
data IVector p t = IVector {
    _vectorGetP1 :: IO p
   ,_vectorGetP2 :: IO p
   ,_vectorPrint :: IO ()
   ,_vectorTail :: Record t
}
DefineSubClass(Vector,Object,IVector,vectorTail,p,,p,2,Typeable p)

-- instantiation checking
c_vector p1 p2 = concrete $ vector p1 p2
   where
   concrete gen = gen
      where
      _ = new gen

vector p1 p2 =
   (vector' `extends` object) noOverride set_Object_Tail
   where
   vector' tail super self = do
      p1r <- newIORef p1
      p2r <- newIORef p2 
      return IVector {
          _vectorGetP1 = readIORef p1r
         ,_vectorGetP2 = readIORef p2r
         ,_vectorPrint = do self # vectorGetP1 >>= ( # pointPrint)
                            self # vectorGetP2 >>= ( # pointPrint)
         ,_vectorTail = tail
      }

vectorGetP1 = _vectorGetP1 . unRecord . get_Object_Tail
vectorGetP2 = _vectorGetP2 . unRecord . get_Object_Tail
vectorPrint = _vectorPrint . unRecord . get_Object_Tail

-- example of width subtyping
norm v = do
   p1 <- v # vectorGetP1 ; p2 <- v # vectorGetP2
   x1 <- p1 # getX ; x2 <- p2 # getX
   return (abs (x1 - x2))

testVector = do
   p1  <- new $ printable_point 0
   p2  <- new $ printable_point 5
   cp1 <- new $ colored_point 10 "red"
   cp2 <- new $ colored_point 25 "red"

   v <- new $ vector p1 p2
   cv <- new $ vector cp1 cp2
   putStrLn "Length of v"
   norm v >>= P.print
   putStrLn "Length of colored cv"
   norm cv >>= P.print

   return ()

-- OOHaskell 5.10
data IVector1 a t = IVector1 {
     _vector1Move0 :: a -> IO ()
   , _vector1Tail :: Record t
}
DefineSubClass(Vector1,Vector,IVector1,vector1Tail,p b,p,b,2,(Typeable p, Typeable b))

vector1Move0 = _vector1Move0 . unRecord . get_Vector_Tail

vector1 p1 p2 = 
   (vector1' `extends` vector p1 p2) noOverride set_Vector_Tail
   where
   vector1' tail super self =
      return IVector1 {
         _vector1Move0 = \pa -> do
            -- leaving this out makes the type of vector1 less polymorph 
            p1 <- self # vectorGetP1
            x <- pa # getX
            p1 # moveX $ x
         ,_vector1Tail = tail
      }

move_origin_to_0 varg = do
   zero <- new $ printable_point 0
   varg # vector1Move0 $ zero

data IVector2 p t = IVector2 {
    _vector2GetP1 :: IO p
   ,_vector2GetP2 :: IO p
   ,_vector2Print :: IO ()
   ,_vector2Set0 :: p -> IO ()
   ,_vector2Tail :: Record t
}
DefineSubClass(Vector2,Object,IVector2,vector2Tail,p,,p,2,(Typeable p))

vector2 p1 p2 =
   (vector2' `extends` object) noOverride set_Object_Tail
   where
   vector2' tail super self = do
      p1r <- newIORef p1
      p2r <- newIORef p2 
      return IVector2 {
          _vector2GetP1 = readIORef p1r
         ,_vector2GetP2 = readIORef p2r
         ,_vector2Print = do self # vector2GetP1 >>= ( # pointPrint)
                             self # vector2GetP2 >>= ( # pointPrint)
         ,_vector2Set0 = writeIORef p1r 
         ,_vector2Tail = tail
      }

v2Meth = unRecord . get_Object_Tail
vector2GetP1 = _vector2GetP1 . v2Meth
vector2GetP2 = _vector2GetP2 . v2Meth
vector2Set0 = _vector2Set0 . v2Meth

align_origins va vb = do
   pa <- va # vector2GetP1
   vb # vector2Set0 $ pa

set_origin_to_0 varg = do
   zero <- new $ printable_point 0
   varg # vector2Set0 $ zero

testAlignOrigin = do
   p1' <- p1   
   p2' <- p2
   cp1' <- cp1
   cp2' <- cp2

   v2 <- new $ vector2 p1' p2'
   cv2 <- new $ vector2 cp1' cp2'
 
   align_origins cv2 cv2
   align_origins v2 v2

   set_origin_to_0 v2
   -- illegal
   --set_origin_to_0 cv2

   return ()

testVector1 = do
   pv1' <- pv1
   cv1' <- cv1
   move_origin_to_0 pv1'
   move_origin_to_0 cv1'
   return ()

p1 = new $ printable_point 0
p2 = new $ printable_point 5

pv = do
   p1' <- p1
   p2' <- p2
   new $ vector p1' p2'

cp1 = new $ colored_point 10 "red"
cp2 = new $ colored_point 25 "red"

cv = do
   cp1' <- cp1
   cp2' <- cp2
   new $ vector cp1' cp2'

pv1 = do
   p1' <- p1
   p2' <- p2
   new $ vector1 p1' p2'

cv1 = do
   cp1' <- cp1
   cp2' <- cp2
   new $ vector1 cp1' cp2'

testVector' = do
   pv'  <- pv
   cv'  <- cv
   cv1' <- cv1
   pv1' <- pv1

   norm pv' >>= P.print
   norm cv' >>= P.print

   norm cv1' >>= P.print
   norm pv1' >>= P.print

   move_origin_to_0 cv1'
   move_origin_to_0 pv1'

   let invariantList :: [Vector ColoredPoint]
       invariantList = consUb cv' (consUb cv' nilUb)
   
   (sequence_ . map (\v -> v # vectorGetP1 >>= (# pointPrint))) invariantList

   cv' # vectorGetP1 >>= \p -> p # coloredPointGetColor >>= P.print
   putStrLn "Length of v"
   norm pv' >>= P.print
   putStrLn "Length of colored cv"
   norm cv' >>= P.print
   return ()

test_pp = do
   p <- new (printable_point 7)
   p # moveX $ 2
   p # getX
   p # pointPrint

myColoredOOP = do
   p <- new (colored_point 5 "red")
   x <- p # getX
   c <- p # coloredPointGetColor
   P.print (x,c)

