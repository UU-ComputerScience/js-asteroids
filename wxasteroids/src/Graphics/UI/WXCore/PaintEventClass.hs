{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.PaintEventClass where

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

data PaintEventClass a = PaintEventClass {
   _paintEventTail :: Record a
}

DefineSubClass(PaintEvent,Event,PaintEventClass,paintEventTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(PaintEventClass,paintEventTc,"PaintEvent")
#endif