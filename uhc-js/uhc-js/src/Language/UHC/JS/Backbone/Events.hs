module Language.UHC.JS.Backbone.Events where

import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Backbone.Model
import Language.UHC.JS.Primitives
import Language.UHC.JS.Types
import Language.UHC.JS.Marshal

bind :: JSAny a -> String -> JSFunction_ b -> IO ()
bind p s = _bind p (toJS s)

foreign import js "%1.bind(%*)"
  _bind :: JSAny a -> JSString -> JSFunction_ b -> IO ()

bind' :: JSAny a -> String -> JSFunction_ b -> JSAny b -> IO ()
bind' p s c = _bind' p (toJS s) c

foreign import js "%1.bind(%*)"
  _bind' :: JSAny a -> JSString -> JSFunction_ b -> JSAny b -> IO ()


foreign import js "%1.unbind()"
  unbind :: JSAny a -> IO ()


unbind' :: JSAny a -> String -> IO ()
unbind' p s = _unbind' p (toJS s)

foreign import js "%1.unbind(%*)"
  _unbind' :: JSAny a -> JSString -> IO ()

unbind'' :: JSAny a -> String -> JSFunction_ b -> IO ()
unbind'' p s f = _unbind'' p (toJS s) f

foreign import js "%1.unbind(%*)"
  _unbind'' :: JSAny a -> JSString -> JSFunction_ b -> IO ()


trigger :: JSAny a -> String -> IO ()
trigger p s = _trigger p (toJS s)

foreign import js "%1.trigger(%*)"
  _trigger :: JSAny a -> JSString -> IO ()

trigger' :: JSAny a -> String -> JSObject_ b -> IO ()
trigger' p s o = _trigger' p (toJS s) o

foreign import js "%1.trigger(%*)"
  _trigger' :: JSAny a -> JSString -> JSObject_ b -> IO ()
-- etc.
