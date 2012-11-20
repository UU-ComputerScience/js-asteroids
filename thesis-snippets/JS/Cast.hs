{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Cast where
import Unsafe.Coerce

class GetObjectRef a where
   getObjectRef :: a -> b

data JSObject_ a

data Node_ a
data Element_ a

type Node a = JSObject_ (Node_ a)
type Element a = JSObject_ (Element_ a)

instance GetObjectRef (Node ()) where
   getObjectRef = undefined 

instance GetObjectRef (Element ()) where
   getObjectRef = undefined

instance GetObjectRef (JSObject_ ()) where
   getObjectRef = undefined

cast :: forall a b. GetObjectRef b => a -> Maybe b
cast a = 
  if instanceOf a (getObjectRef (undefined :: b))
    then Just (unsafeCoerce a)
    else Nothing

instanceOf :: a -> b -> Bool
instanceOf = undefined

