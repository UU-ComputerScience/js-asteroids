{-# LANGUAGE CPP, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances #-}
{-# OPTIONS -pgmP cpp #-}
module LightOO (
   module LightOO.Core
   ,instanceof
   ,returnIO
   ,singleton
#ifndef __UHC__
   ,Castable(..)
   ,CastCons(..)
#else
   ,consUb
   ,nilUb
   ,consLb
   ,nilLb
#endif
   -- object stuff
   ,IObject
   ,object
   ,sameObject
   ,objectMethods
   ,Object
   ,Object_
   ,get_Object_Tail
   ,set_Object_Tail
   ,modify_Object_Tail
) where

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else
#include "LightOO.h"
#endif

import LightOO.Core
import Data.Typeable
import Data.Maybe
import Data.Array
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Data.IORef
import Prelude hiding (foldr)
#ifndef __UHC__
import qualified Data.Map as M
#endif
import qualified Data.Array as A

#ifndef __UHC__
class Functor f => Castable f where
   fup :: (Sub a b) => f a -> f b
   fup = fmap upcast

   fdown :: forall a b. (Foldable f, Applicative f, Monoid (f a), Sup b a) => f b -> f a
   fdown = foldr (mappend . maybe mempty pure . (downcast :: b -> Maybe a)) mempty
   
instance Castable []
instance Castable (M.Map k)
instance Ix i => Castable (A.Array i)
instance Castable Maybe

class Applicative f => CastCons f where
   consUb :: forall a b. (Typeable b, Sub a b, Monoid (f b)) => a -> f b -> f b
   consUb o xs = pure (upcast o :: b) `mappend` xs

   consLb :: forall b a. (Typeable b, Sup b a, Monoid (f a)) => b -> f a -> f a
   consLb o xs = maybe xs (mappend xs . pure) (downcast o :: Maybe a)

   nilUb,nilLb  :: Monoid (f a) => f a 
   nilUb = mempty
   nilLb = mempty

instance CastCons []
instance CastCons Maybe
#else
consUb :: (Typeable a, Typeable b, Sub a b) => a -> [b] -> [b]
consUb o (xs :: [b]) = (upcast o :: b) : xs

nilUb :: (Typeable a, Sub a a) => [a]
nilUb = []

consLb :: (Typeable a, Typeable b, Sup b a) => b -> [a] -> [a]
consLb o (xs :: [a]) = 
   case downcast o :: Maybe a of
      Just x  -> x : xs
      Nothing -> xs

nilLb :: (Typeable a, Sup a a) => [a]
nilLb = []
#endif

instanceof :: (Sup b a) => b -> a -> Bool
instanceof b (_ :: a) = isJust (downcast b :: Maybe a)

singleton :: IORef (Maybe o) -> ClosedClass o -> IO o
singleton store o = do
   s <- readIORef store
   let storeInstance = do
         inst <- new o
         writeIORef store (Just inst)
         return inst
   maybe storeInstance return s
   
data IObject t = IObject {
    objectGetFlag :: IO Bool
   ,objectSetFlag :: Bool -> IO ()
   ,_objectTail :: Record t
}
DefineClass(Object,IObject,objectTail,,1)

#ifdef __UHC__
INSTANCE_TYPEABLE1(IObject,objectTc,"Object")
#endif

object = clazz $ \tail self -> do
   flag <- newIORef False
   return IObject {
       objectSetFlag = writeIORef flag
      ,objectGetFlag = readIORef flag
      ,_objectTail = tail
   }

sameObject a b = do
   a # objectSetFlag $ False
   b # objectSetFlag $ True
   a # objectGetFlag

returnIO = return :: a -> IO a

objectMethods = id