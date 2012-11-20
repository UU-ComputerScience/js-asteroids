{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Graphics.UI.WXCore.PaintEvent (
    module Graphics.UI.WXCore.PaintEventClass
   ,paintEvent
) where

import LightOO
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.PaintEventClass
import Graphics.UI.WXCore.Event

paintEvent id =
   (paintEvent' `extends` event id wxEVT_PAINT) noOverride set_Event_Tail
   where
   paintEvent' tail super self = 
      return PaintEventClass {
         _paintEventTail = tail
      } 
