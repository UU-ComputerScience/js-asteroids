{-# LANGUAGE CPP, MultiParamTypeClasses, StandaloneDeriving, FlexibleInstances, TypeOperators, OverlappingInstances, ScopedTypeVariables #-}
{-# OPTIONS -pgmP cpp #-}
module UnionExt where

import Unsafe.Coerce
import Prelude hiding (log)
import Data.Maybe

data JSUndefined
data JSNull

data JSObject a

deriving instance Show (JSObject a)

data JSBool_
type JSBool = JSObject JSBool_

data Element_ a
type Element a = JSObject (Element_ a)

data Node_ a
type Node a = Element (Node_ a)

data CHTMLElement a
type HTMLElement_ a = Node (CHTMLElement a)

data Document_
type Document = Node Document_

data JSString_
#ifdef __UHC__
type JSString = JSObject PackedString
#else
type JSString = JSObject JSString_
#endif

data a :|: b = L a | R b
infixr 5 :|:

class SubType sub sup where
   inj :: sub -> sup          -- injection
   prj :: sup -> Maybe sub    -- projection


instance SubType a a where
  inj = id
  prj = Just

instance SubType a (a :|: b) where
  inj        = L
  prj (L x)  = Just x
  prj _      = Nothing

instance SubType a c => SubType a (b :|: c) where
  inj        = R . inj
  prj (R x)  = prj x
  prj _      = Nothing

x :: Int :|: String :|: Bool
x = inj True

bar :: JSNull :|: Int -> JSString :|: JSBool
bar a = 
   let jsVal = 
         case prj a :: Maybe JSNull of
         	Just v  -> unsafeCoerce v
         	Nothing -> 
         		case prj a :: Maybe Int of
         			Just v  -> unsafeCoerce v
         			Nothing -> error "impossible"

       ret r | isString r = inj (unsafeCoerce r :: JSString)
             | isBool   r = inj (unsafeCoerce r :: JSBool)
   in ret (_bar jsVal)

fooBar :: JSNull :|: Int -> JSString :|: JSBool -> JSBool :|: Int
fooBar = undefined

test :: forall a. Node a :|: Bool -> IO ()
test n = do
   let node = prj n :: Maybe (Node a)
   print (isJust node)
   putStr "yo"

foo :: forall a. IO ()
foo = test (inj (undefined :: Node a) :: Node a :|: Bool)

fubar = \(x :: ((JSObject a) :|: Bool)) -> True
fubar1 = \(x :: ((Element a) :|: Bool)) -> True

test1 = fubar (inj (undefined :: JSObject ()) :: (JSObject () :|: Bool))
test2 :: forall a. Bool
test2 = fubar1 (inj (undefined :: Node a) :: (Node a :|: Bool)) 


($<) :: (SubType c a, SubType d b) => (a -> b) -> c -> Maybe d
($<) f = prj . f . inj

#ifdef __UHC__
foreign import js "primIsBool(%1)"
   isBool :: a -> Bool

foreign import js "primIsString(%*)"
   isString :: a -> Bool

foreign import js "bar(%1)" 
   _bar :: a -> b

foreign import js "null"
	_null :: JSNull

foreign import js "console.log(%*)"
	log :: a -> IO ()
#else
_bar :: a -> b
_bar = undefined

_null :: JSNull
_null = undefined

log :: a -> IO ()
log = undefined

isString :: a -> Bool 
isString = undefined

isBool :: a -> Bool
isBool = undefined
#endif

main = do
    let Just v = (bar $< _null) :: Maybe JSString
    log v
