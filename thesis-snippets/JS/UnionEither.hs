module UnionEither where

data JSString
data JSBool

-- todo fix to real functions once these are in the rts
foreign import js "primIsString(%*)"
   isString :: a -> Bool

foreign import js "primIsBool(%*)"
   isBool :: a -> Bool

foo :: a -> Either JSString JSBool
foo a =
   let ret r  | isString  r  = Left (unsafeCoerce r)
              | isBool    r  = Right (unsafeCoerce r)
   in ret (_foo a)

foreign import js "foo(%1)" 
   _foo :: a -> b