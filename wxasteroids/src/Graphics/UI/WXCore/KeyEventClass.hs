{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.KeyEventClass where

import LightOO
import Data.Typeable
import Data.IORef
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.EventClass

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else
#include "LightOO.h"
#endif

data KeyEventClass a = KeyEventClass {
   -- only used for modifiers
    --_keyboardState :: KeyboardState
    _keyEventGetKeyCode :: IO Int
   ,_keyEventGetX :: IO Int
   ,_keyEventGetY :: IO Int
   ,_keyEventGetUnicodeKey :: IO String
   ,_keyEventTail :: Record a
}

DefineSubClass(KeyEvent,Event,KeyEventClass,keyEventTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(KeyEventClass,keyEventTc,"KeyEvent")
#endif

keyEvent_methods = unRecord . get_Event_Tail
keyEventGetKeyCode = _keyEventGetKeyCode . keyEvent_methods
keyEventGetX = _keyEventGetX . keyEvent_methods
keyEventGetY = _keyEventGetY . keyEvent_methods
keyEventGetUnicodeKey = _keyEventGetUnicodeKey . keyEvent_methods