{-
http://docs.wxwidgets.org/trunk/classwx_event.html
-}
{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.EventClass where

import LightOO
import Data.Typeable
import Graphics.UI.WXCore.Types

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else
#include "LightOO.h"
#endif

data EventClass a = EventClass {
    _eventGetType :: IO EventType
   ,_eventGetId :: IO Id
   ,_eventGetSkipped :: IO Bool
   ,_eventGetEventObject :: IO (Maybe Object)
   ,_eventSetEventObject :: Object -> IO ()
   ,_eventSkip :: Bool -> IO ()
   ,_eventStopPropagation :: IO ()
   ,_eventTail :: Record a
}
DefineSubClass(Event,Object,EventClass,eventTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(EventClass,eventTc,"Event")
#endif

event_Methods = unRecord . get_Object_Tail
eventGetType = _eventGetType . event_Methods
eventGetId = _eventGetId . event_Methods
eventSkip = _eventSkip . event_Methods
eventGetEventObject = _eventGetEventObject . event_Methods
eventSetEventObject = _eventSetEventObject . event_Methods