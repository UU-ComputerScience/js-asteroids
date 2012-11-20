{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module  Graphics.UI.WXCore.TimerClass where

import LightOO
import Data.Typeable
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.EvtHandlerClass
import Graphics.UI.WXCore.EventClass

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else 
#include "LightOO.h"
#endif

data TimerEventClass a = TimerEventClass {
    _timerEventGetInterval :: IO Int
   ,_timerEventGetTimer :: IO Timer
   ,_timerEventTail :: Record a
}
DefineSubClass(TimerEvent,Event,TimerEventClass,timerEventTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(TimerEventClass,timerEventTc,"TimerEvent")
#endif

timerEvent_Methods = unRecord . get_Event_Tail
timerEventGetInterval = _timerEventGetInterval . timerEvent_Methods
timerEventGetTimer = _timerEventGetTimer . timerEvent_Methods

data TimerClass a = TimerClass {
    _timerGetId :: IO Int
   ,_timerGetInterval :: IO Int
   ,_timerGetOwner :: IO EvtHandler
   ,_timerIsOneShot :: IO Bool
   ,_timerIsRunning :: IO Bool
   ,_timerSetOwner :: EvtHandler -> Id -> IO ()
   ,_timerStart :: Int -> Bool -> IO Bool
   ,_timerStop :: IO ()
   ,_timerTail :: Record a
}
DefineSubClass(Timer,EvtHandler,TimerClass,timerTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(TimerClass,timerTc,"Timer")
#endif

timer_Methods = unRecord . get_EvtHandler_Tail

timerIsRunning = _timerIsRunning . timer_Methods 
timerStop = _timerStop . timer_Methods
timerIsOneShot = _timerIsOneShot . timer_Methods
timerStart = _timerStart . timer_Methods
timerGetOwner = _timerGetOwner . timer_Methods
timerGetInterval = _timerGetInterval . timer_Methods

-- static methods

-- TODO: not really sure what this is for..?
timerGetOnCommand :: Timer_ a -> IO (IO ())
timerGetOnCommand t = do
   owner <- t # timerGetOwner
   cb <- do { cd <- (owner # evtHandlerGetHandler) wxEVT_TIMER idAny idAny
            ; return $ maybe (const $ return ()) id cd
            }
   return $ cb (error "touched: event object")
   
timerOnCommand :: Timer_ a -> IO () -> IO ()
timerOnCommand t f = do
   owner <- t # timerGetOwner
   (owner # evtHandlerBind) wxEVT_TIMER (const f) idAny idAny