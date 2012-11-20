module Graphics.UI.WXCore.Event (
   module Graphics.UI.WXCore.EventClass
  ,event
) where

import LightOO
import Graphics.UI.WXCore.EventClass
import Data.IORef

event id eventType =
   (event' `extends` object) noOverride set_Object_Tail
   where
   event' tail super self = do
      source <- newIORef Nothing
      return EventClass {
          _eventGetType = return eventType
         ,_eventGetId = return id
         ,_eventGetSkipped = return False
         ,_eventGetEventObject = readIORef source 
         ,_eventSetEventObject = writeIORef source . Just
         ,_eventSkip = \_ -> return ()
         ,_eventStopPropagation = return () 
         ,_eventTail = tail
      }