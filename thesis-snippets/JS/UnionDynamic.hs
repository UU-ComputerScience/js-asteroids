{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, Rank2Types #-}
{-# OPTIONS -pgmP cpp #-}
module UnionDynamic where

import Data.Dynamic
import Data.Maybe
import Control.Monad (mplus)
import Data.Typeable
import Unsafe.Coerce
import Prelude hiding (log)

#ifdef __UHC__
#include "Typeable.h"
#endif

data JSUndefined
data JSNull

data JSObject a

data JSBool_
type JSBool = JSObject JSBool_

data Element_ a
type Element a = JSObject (Element_ a)

data Node_ a
type Node a = Element (Node_ a)

data JSString_
#ifdef __UHC__
type JSString = JSObject PackedString
#else
type JSString = JSObject JSString_
#endif

data TyVar

#ifdef __UHC__
INSTANCE_TYPEABLE0(TyVar,tyVarTc,"TyVar")
INSTANCE_TYPEABLE0(JSNull,jsNullTc,"JSNull")
INSTANCE_TYPEABLE0(JSBool_,jsBoolTc,"JSBool")
INSTANCE_TYPEABLE0(PackedString,jsPackedStringTc,"PackedString")
INSTANCE_TYPEABLE1(JSObject,jsObjectTc,"JSObject")
INSTANCE_TYPEABLE1(Element_,jsElementTc,"Element")
INSTANCE_TYPEABLE1(Node_,jsNodeTc,"Node")
#else
deriving instance Typeable TyVar
deriving instance Typeable JSNull
deriving instance Typeable JSBool_
deriving instance Typeable JSString_
deriving instance Typeable1 JSObject
deriving instance Typeable1 Element_
deriving instance Typeable1 Node_
#endif

bar :: Dynamic -> Dynamic 
bar d =
    let jsVal =
            case fromDynamic d :: Maybe JSNull of
                Just v  -> unsafeCoerce v
                Nothing -> case fromDynamic d :: Maybe Int of
                            Just v  -> unsafeCoerce v
                            Nothing -> error "impossible"

        ret r |  isString  r  = toDyn (unsafeCoerce r :: JSString)
              |  isBool    r  = toDyn (unsafeCoerce r :: JSBool)
    in ret (_bar jsVal)


getNodeType :: Dynamic -> IO Int
getNodeType d =
    let jsVal =
            case fromDynamic d :: Maybe JSNull of
                Just v  -> unsafeCoerce v
                Nothing -> case prjNode d :: Maybe (Node a) of
                            Just v  -> unsafeCoerce v
                            Nothing -> error "impossible"
    in _getNodeType jsVal

getNodeOrNull :: JSBool -> IO Dynamic
getNodeOrNull b = do 
    r <- _getNodeOrNull b 
    if isNode r 
      then return $ injNode (unsafeCoerce r)
      else return $ toDyn (undefined :: JSNull)

class Iso f where
  inj :: f a -> Dynamic
  prj :: Dynamic -> Maybe (f a)
--  prj :: Dynamic -> exists a. f a

newtype Two a b x = Two { unTwo :: a (b x) }

instance (Typeable1 a, Typeable1 b) => Iso (Two a b) where
  inj   = toDyn . (unsafeCoerce :: a (b x) -> a (b TyVar)) . unTwo
  prj d = case fromDynamic d :: Maybe (a (b TyVar)) of
            Nothing -> Nothing
            Just x  -> Just (Two $ unsafeCoerce x)

injNode :: Node a -> Dynamic
injNode = toDyn . (unsafeCoerce :: Node a -> Node TyVar)

#ifdef __UHC__
prjNode :: Dynamic -> exists a. Maybe (Node a)
prjNode = unsafeCoerce . (fromDynamic :: Dynamic -> Maybe (Node TyVar))

foreign import js "rts.js primIsBool(%1)"
   isBool :: a -> Bool

foreign import js "primIsString(%*)"
   isString :: a -> Bool

foreign import js "bar(%1)" 
   _bar :: a -> b

foreign import js "null"
	_null :: JSNull

foreign import js "true"
   _true :: JSBool

foreign import js "console.log(%*)"
	log :: a -> IO ()

foreign import js "getNodeType(%*)"
    _getNodeType :: a -> IO Int

foreign import js "document"
    _document :: IO (Node ())

foreign import js "getNodeOrNull(%1)" 
    _getNodeOrNull :: JSBool -> IO a

foreign import js "primIsInstanceOf(%1, Node)"
    isNode :: a -> Bool
#else
prjNode :: Dynamic ->Maybe (Node a)
prjNode = undefined

_bar :: a -> b
_bar = undefined

_null :: JSNull
_null = undefined

log :: a -> IO ()
log = undefined

_getNodeType :: a -> IO Int
_getNodeType = undefined

_getNodeOrNull :: JSBool -> IO a
_getNodeOrNull = undefined

_true = undefined

isNode = undefined

_document :: IO (Node ())
_document = undefined

isString :: a -> Bool 
isString = undefined

isBool :: a -> Bool
isBool = undefined
#endif

logNodeType :: Node a -> IO ()
logNodeType n = getNodeType (injNode n) >>= log

main = do
    let x = toDyn _null
    n <- getNodeOrNull _true
    let Just v = fromDynamic (bar x) :: Maybe JSString
    log v
    logNodeType (fromJust $ prjNode n)
