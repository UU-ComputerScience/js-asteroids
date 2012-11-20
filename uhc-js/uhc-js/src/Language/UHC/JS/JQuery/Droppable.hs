module Language.UHC.JS.JQuery.Droppable where

import Language.UHC.JS.Prelude
import Language.UHC.JS.Types
import Language.UHC.JS.JQuery.JQuery

data Droppable = Droppable { hoverClass :: JSString,
                             drop       :: JUIEventHandler}

data JSDroppablePtr
type JSDroppable = JSObject_ JSDroppablePtr

droppable :: JQuery -> Droppable -> IO ()
droppable jq drop =
  do jsdrop <- mkObj drop
     _droppable jq jsdrop
      
foreign import js "%1.droppable(%2)"
  _droppable :: JQuery -> JSDroppable -> IO ()