{-# LANGUAGE MultiParamTypeClasses #-}
module Language.UHC.JS.Types where

import Control.Monad
import UHC.BoxArray (BoxArray)

data JSAny a

data JSUndefined_
type JSUndefined = JSAny JSUndefined_

data JSNull_
type JSNull = JSAny JSNull_

data CJSObject_ a
type JSObject_ a = JSAny (CJSObject_ a)
type JSObject = JSObject_ ()

data JSBool_
type JSBool = JSObject_ JSBool_

type JSString = JSObject_ PackedString

data CJSFunction_ a
type JSFunction_ a = JSObject_ (CJSFunction_ a)

data JSRegex_
type JSRegex = JSObject_ JSRegex_

type JSArray v = JSObject_ (BoxArray v)

foreign import js "null"
   _null :: JSNull

foreign import js "undefined"
   _undefined :: JSUndefined

foreign import js "true"
  _true :: JSBool
  
foreign import js "false"
  _false :: JSBool

foreign import js "''"
  _string :: JSString

foreign import js "new Array()"
  _array :: JSArray k a