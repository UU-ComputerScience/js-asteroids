{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Examples.Triplet where

#ifdef __UHC__
#include "../LightOOUHC.h"
#include "Typeable.h"
#else
#include "../LightOO.h"
#endif

import LightOO
import Data.Typeable

data IPair a b t = IPair {
    _pairGetFirst   :: IO a
   ,_pairGetSecond  :: IO b
   ,_pairTail       :: Record t
}

DefineSubClass(Pair,Object,IPair,pairTail,a b,,a b,3,(Typeable a, Typeable b))

#ifdef __UHC__
INSTANCE_TYPEABLE3(IPair,pairTc,"Pair")
#endif

pairMethods = unRecord . get_Object_Tail
pairGetFirst =  _pairGetFirst . pairMethods
pairGetSecond =  _pairGetSecond . pairMethods

pair a b = 
   (pair' `extends` object) noOverride set_Object_Tail
   where
   pair' tail super self = 
      return IPair {
          _pairGetFirst  = return a
         ,_pairGetSecond = return b
         ,_pairTail      = tail
      }

data ITriplet a b c t = ITriplet {
    _tripletGetThird    :: IO c
   ,_tripletSwap        :: IO (Triplet b a c)
   ,_tripletTail        :: Record t
}

DefineSubClass(Triplet,Pair,ITriplet,tripletTail,a b c,a b,a b c,4,(Typeable a, Typeable b, Typeable c))

#ifdef __UHC__
INSTANCE_TYPEABLE4(ITriplet,tripletTc,"Triplet")
#endif
 
tripletGetThird = _tripletGetThird . unRecord . get_Pair_Tail
tripletSwap = _tripletSwap . unRecord . get_Pair_Tail

triplet
  :: a
     -> b
     -> c
     -> OpenClass (Record tail) self (Pair_ a b (ITriplet a b c tail)) 
triplet a b c = 
   (triplet' `extends` pair a b) noOverride set_Pair_Tail
   where
   triplet' tail super self = 
      return ITriplet {
          _tripletGetThird  = return c
         ,_tripletSwap      = new $ triplet b a c
         ,_tripletTail      = tail
      }

myOOPair = do
   p <- new $ pair (0 :: Int) (3.0 :: Double)
   let o = upcast p :: Object
   let Just p = downcast o :: Maybe (Pair Int Double)
   p # pairGetFirst >>= putStrLn . show
   t <- new $ triplet (0 :: Int) (1 :: Int) (2 :: Int) 
   return ()

projectPair :: Pair (Pair a b) c -> IO (Pair a b)
projectPair p = p # pairGetFirst

test :: Triplet a b String -> IO ()
test o = o # tripletGetThird >>= putStrLn

myOOTriplet' = do
  p <- new $ pair (0 :: Int) (3.0 :: Double)
  t <- new $ triplet (0 :: Int) (4.0 :: Double) "Hi"

  let pairs  :: [Pair Int Double]
      pairs  = consUb t (consUb p nilUb)

  sequence_ $ map (\p -> p # pairGetFirst >>= print) pairs
  
  t' <- t # tripletSwap
  t' # pairGetFirst >>= print

myOOTriplet = do
   p <- new $ pair (0 :: Int) (3.0 :: Double)
   p # pairGetFirst >>= print
   nestedPair <- new $ pair p "Hi"
   nestedPair # pairGetSecond >>= putStrLn

   (p `sameObject` p) >>= print
   (p `sameObject` nestedPair) >>= print

   let o = upcast p :: Object
  
   let Just p' = downcast o :: Maybe (Pair Int Double)
   p' # pairGetFirst >>= print
   
   t <- new $ triplet (0 :: Int) (4.0 :: Double) "Hi"
   let tp = upcast t :: Pair Int Double
   let o  = upcast t :: Object

   let t' = downcast o :: Maybe (Pair Int Double)
   let Just t'' = downcast o :: Maybe (Triplet Int Double String)
   t'' # tripletGetThird >>= putStrLn

   let xs :: [Pair Int Double]
       xs = consUb t'' (consUb p nilUb)

   sequence_ $ map (\p -> p # pairGetFirst >>= print) xs
   test t''
   
   --let sillydown = downcast t'' :: Maybe Object

   t'' # tripletSwap
