module Language.UHC.JS.JQuery.Draggable where

import Language.UHC.JS.Prelude
import Language.UHC.JS.Types
import Language.UHC.JS.JQuery.JQuery

data Draggable = Draggable { scroll :: JSBool, containment    :: JSString,
                             revert :: JSBool, revertDuration :: Int,
                             scrollSensitivity :: Int,
                             start :: JUIEventHandler}

data JSDraggablePtr
type JSDraggable = JSObject_ JSDraggablePtr

draggable :: JQuery -> Draggable -> IO ()
draggable jq drag =
  do jsdrag <- mkJSDraggable drag
     _draggable jq jsdrag
      
foreign import js "{}"
  mkJSDraggable :: Draggable -> IO JSDraggable

foreign import js "%1.draggable(%2)"
  _draggable :: JQuery -> JSDraggable -> IO ()
