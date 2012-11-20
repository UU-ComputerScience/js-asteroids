{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Substitution where
import Unsafe.Coerce

class Sub a b where
   upcast :: a -> b

-- depth 1

instance Sub (a ()) (a ()) where
   upcast = id

-- depth 2

instance Sub (a (b ())) (a (b ())) where
   upcast = id


instance (Sub (a ()) x) => Sub (a (b ())) x where
   upcast = upcast . (undefined :: a (b ()) -> a () )

-- depth 3

instance Sub (a (b (c ()))) (a (b (c ()))) where
   upcast = id

instance (Sub (a (b ())) x) => Sub (a (b (c ()))) x where
   upcast = upcast . (undefined :: a (b (c ())) -> a (b ()))



data A a
data B a
data C a

--subst :: A () -> A (forall a. a)
subst :: A () -> A a
subst = unsafeCoerce

subst' :: A (forall a. a) -> A ()
subst' = unsafeCoerce

test :: A a -> IO ()
test a = putStrLn "Hi"

_subst :: a () -> a (forall t. t)
_subst = unsafeCoerce

newtype CW a = CW { unC :: A (B (C a)) }


trans :: A () -> A ()
trans a = 
   let f :: (Sub a (A ())) => a -> A ()
       f = upcast

       g :: A () -> A a
       g = subst

   in f (g a)

main = do
   let a = undefined :: A ()
   let c = undefined :: A (B (C ()))
   let x = subst a -- :: A Bool 
   let y = subst' x
   test x
   test y
   let c' = unC $ _subst (CW c)
   return c'

