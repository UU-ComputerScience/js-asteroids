{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Graphics.UI.WXCore.KeyEvent (
    module Graphics.UI.WXCore.KeyEventClass
   ,keyEvent
) where

import LightOO
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.KeyEventClass
import Graphics.UI.WXCore.Event

keyEvent id x y keyCode unicode =
   (keyEvent' `extends` event id wxEVT_CHAR) noOverride set_Event_Tail
   where
   keyEvent' tail super self = 
      return KeyEventClass {
          _keyEventGetKeyCode = return keyCode
         ,_keyEventGetX = return x
         ,_keyEventGetY = return y
         ,_keyEventGetUnicodeKey = return unicode
         ,_keyEventTail = tail
      } 
