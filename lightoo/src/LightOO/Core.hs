{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
module LightOO.Core (
    Record
   ,record
   ,unRecord
   ,hideRecord
   ,(#)
   ,Class
   ,ClosedClass
   ,new
   ,genericWiden
   ,clazz
   ,extends
   ,noOverride
   ,mkMod
   ,Sub(..)
   ,Sup(..)
   ,Widen(..)
   ,Narrow(..)
   ,ModTail(..)
) where

import Data.Maybe
import Data.Dynamic
import Control.Monad.Fix (mfix)

type HiddenRecord = Dynamic

type Record a = Either a HiddenRecord

type AbstractClass tail self o = tail -> self -> IO o

type EmptyRecord = IO (Record ())

type Class tail self o = AbstractClass (IO tail) self o

type ClosedClass o = AbstractClass EmptyRecord o o

type SuperClass self sup = AbstractClass EmptyRecord self sup

type SubClass tail sup self sub = tail -> sup -> self -> IO sub

emptyRecord :: EmptyRecord
emptyRecord = return (record ())

record = Left

unRecord (Left o) = o

hideRecord :: Typeable a => Record a -> Record ()
hideRecord (Left o) = Right (toDyn o)
hideRecord _        = error "invariant: cannot hide an already hidden record"

restoreRecord :: Typeable a => HiddenRecord -> Maybe a
restoreRecord = fromDynamic

o # f = f o

class Sub a b where
   upcast :: a -> b

class Sup a b where
   downcast :: a -> Maybe b

class Narrow a b where
   narrow :: a -> b

class Widen a b where
   widen :: a -> Maybe b

class ModTail f where
   getTail :: f a -> Record a
   setTail :: f a -> Record b -> f b

   modifyTail :: (Record a -> Record b) -> f a -> f b
   modifyTail = mkMod setTail getTail

mkMod set get f o = set o (f (get o)) 

genericWiden :: Typeable b => o -> (o -> Record a) -> (o -> Record b -> c) -> Maybe c
genericWiden o getTail (setTail :: f -> Record b -> c) =
   case getTail o of
      Right d -> maybe Nothing (Just . setTail o . record) (restoreRecord d :: Maybe b)
      Left  _ -> error "invariant broken: nil case should be caught by a reflexivity instance"

-- Type signature breaks UHC, type synonym expansion going wrong?
--new :: ClosedClass o -> IO o
new c = mfix $ c emptyRecord

clazz :: AbstractClass tail self o -> Class tail self o
clazz cont tail self = tail >>= \t -> cont t self

extends ::
        SubClass tail sup self sub
     -> SuperClass self sup
     -> (sup -> self -> IO sup')
     -> (sup' -> Record sub -> o)
     -> Class tail self o
extends w g override oplus = clazz $ \tail self -> do
  super   <- g emptyRecord self
  wrapper <- w tail super self
  super'  <- override super self
  return $ super' `oplus` (record wrapper)

noOverride :: super -> self -> IO super
noOverride sup _ = return sup

-- depth 1

instance Sub (a ()) (a ()) where
   upcast = id

instance Sup (a ()) (a ()) where
   downcast = Just

-- depth 2

instance Sub (a (b ())) (a (b ())) where
   upcast = id

instance Sup (a (b ())) (a (b ())) where
   downcast = Just

instance (Sub (a ()) x, Narrow (a (b ())) (a ())) => Sub (a (b ())) x where
   upcast = upcast . (narrow :: a (b ()) -> a () )

instance (Sup (a (b ())) (a (b c)), Widen (a ()) (a (b ()))) => Sup (a ()) (a (b c)) where
   downcast o = case widen o :: Maybe (a (b ())) of
                  Just r   -> downcast r 
                  Nothing  -> Nothing

-- depth 3

instance Sub (a (b (c ()))) (a (b (c ()))) where
   upcast = id

instance Sup (a (b (c ()))) (a (b (c ()))) where
   downcast = Just

instance (Sub (a (b ())) x, Narrow (a (b (c ()))) (a (b ()))) => Sub (a (b (c ()))) x where
   upcast = upcast . (narrow :: a (b (c ())) -> a (b ()))

instance (Sup (a (b (c ()))) (a (b (c d))), Widen (a (b ())) (a (b (c ())))) => Sup (a (b ())) (a (b (c d))) where
   downcast o = case widen o :: Maybe (a (b (c ()))) of
                  Just r   -> downcast r
                  Nothing  -> Nothing

-- depth 4
instance Sub (a (b (c (d ())))) (a (b (c (d ())))) where
   upcast = id

instance Sup (a (b (c (d ())))) (a (b (c (d ())))) where
   downcast = Just

instance (Sub (a (b (c ()))) x, Narrow (a (b (c (d ())))) (a (b (c ())))) => Sub (a (b (c (d ())))) x where
   upcast = upcast . (narrow :: a (b (c (d ()))) -> a (b (c ())))

instance (Sup (a (b (c (d ())))) (a (b (c (d e)))), Widen (a (b (c ()))) (a (b (c (d ()))))) => Sup (a (b (c ()))) (a (b (c (d e)))) where
   downcast o = case widen o :: Maybe (a (b (c (d ())))) of
                  Just r   -> downcast r
                  Nothing  -> Nothing

-- depth 5
instance Sub (a (b (c (d (e ()))))) (a (b (c (d (e ()))))) where
   upcast = id

instance Sup (a (b (c (d (e ()))))) (a (b (c (d (e ()))))) where
   downcast = Just

instance (Sub (a (b (c (d ())))) x, Narrow (a (b (c (d (e ()))))) (a (b (c (d ()))))) => Sub (a (b (c (d (e ()))))) x where
   upcast = upcast . (narrow :: a (b (c (d (e ())))) -> a (b (c (d ()))))

instance (Sup (a (b (c (d (e ()))))) (a (b (c (d (e f))))), Widen (a (b (c (d ())))) (a (b (c (d (e ())))))) => Sup (a (b (c (d ())))) (a (b (c (d (e f))))) where
   downcast o = case widen o :: Maybe (a (b (c (d (e ()))))) of
                  Just r   -> downcast r
                  Nothing  -> Nothing

-- depth 6
instance Sub (a (b (c (d (e (f ())))))) (a (b (c (d (e (f ())))))) where
   upcast = id

instance Sup (a (b (c (d (e (f ())))))) (a (b (c (d (e (f ())))))) where
   downcast = Just

instance (Sub (a (b (c (d (e ()))))) x, Narrow (a (b (c (d (e (f ())))))) (a (b (c (d (e ())))))) => Sub (a (b (c (d (e (f ())))))) x where
   upcast = upcast . (narrow :: a (b (c (d (e (f ()))))) -> a (b (c (d (e ())))))

instance (Sup (a (b (c (d (e (f ())))))) (a (b (c (d (e (f g)))))), Widen (a (b (c (d (e ()))))) (a (b (c (d (e (f ()))))))) => Sup (a (b (c (d (e ()))))) (a (b (c (d (e (f g)))))) where
   downcast o = case widen o :: Maybe (a (b (c (d (e (f ())))))) of
                  Just r   -> downcast r
                  Nothing  -> Nothing

