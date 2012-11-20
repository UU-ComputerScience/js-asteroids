{-# LANGUAGE CPP #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.Timer (
    module Graphics.UI.WXCore.TimerClass
   ,timerEvent
   ,timer
) where

import LightOO
import Graphics.UI.WXCore.TimerClass
import Data.IORef
import Data.Maybe
import Control.Monad
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.EvtHandler
import Graphics.UI.WXCore.Event
#ifdef __UHC__
import Language.UHC.JS.HTML5.Window
#else
window = undefined
clearInterval = undefined
setInterval = undefined 
#endif

timerEvent id timer = 
   (timerEvent' `extends` event id wxEVT_TIMER) noOverride set_Event_Tail
   where
   timerEvent' tail super self = 
      return TimerEventClass {
          _timerEventGetInterval = self # timerEventGetTimer >>= timerGetInterval
         ,_timerEventGetTimer = return timer
         ,_timerEventTail = tail
      }

timer owner id =
   (timer' `extends` evthandler) noOverride set_EvtHandler_Tail
   where
   timer' tail super self = do 
      interval  <- newIORef 0
      owner     <- newIORef owner
      isone     <- newIORef False
      isRunning <- newIORef False
      i <- if id == idAny then idCreate else return id
      id <- newIORef i
      return TimerClass {
          _timerGetId = readIORef id
         ,_timerGetInterval = readIORef interval 
         ,_timerGetOwner = readIORef owner
         ,_timerIsOneShot = readIORef isone
         ,_timerIsRunning = readIORef isRunning
         ,_timerSetOwner = \newOwner newId -> do
            writeIORef id newId
            writeIORef owner newOwner
         ,_timerStart = \milli oneshot -> do

            writeIORef isone oneshot

            let this :: Timer
                this = upcast self
                
            timerEventId <- idCreate
            timingEvent <- new $ timerEvent timerEventId this
            timingEvent # eventSetEventObject $ (upcast this)

            handler <- readIORef owner
            
            let cb = do {
               ; handler # evtHandlerProcessEvent $ (upcast timingEvent)
               ; when oneshot (self # timerStop)
            }

            w <- window
            timerId <- setInterval w cb milli
            writeIORef isRunning True
            writeIORef id timerId
            return True

         ,_timerStop = do
            w <- window
            timerId <- readIORef id
            clearInterval w timerId
            writeIORef isRunning False
         ,_timerTail = tail
      }

