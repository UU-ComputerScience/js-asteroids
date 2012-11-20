{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.EvtHandlerClass where

import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.EventClass
import LightOO
import Data.Typeable
import Data.IORef

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else
#include "LightOO.h"
#endif

--eVT_MOTION :: EventType
--wxEVT_MOTION = MOUSEMOVE

--wxEVT_ENTER_WINDOW :: EventType
--wxEVT_ENTER_WINDOW = MOUSEOVER

--wxEVT_LEAVE_WINDOW :: EventType
--wxEVT_LEAVE_WINDOW = MOUSEOUT

--wxEVT_LEFT_DOWN :: EventType
--wxEVT_LEFT_DOWN = CLICK

--type Point = (Int,Int)

--data EventMouse
--  =  MouseMotion      !Point !Modifiers -- ^ Mouse was moved over the client area of the window
--  |  MouseEnter       !Point !Modifiers -- ^ Mouse enters in the client area of the window
--  |  MouseLeave       !Point !Modifiers -- ^ Mouse leaves the client area of the window
--  |  MouseLeftDown    !Point !Modifiers -- ^ Mouse left button goes down
--  |  MouseLeftUp      !Point !Modifiers -- ^ Mouse left  button goes up
--  |  MouseLeftDClick  !Point !Modifiers -- ^ Mouse left button double click
--  |  MouseLeftDrag    !Point !Modifiers -- ^ Mouse left button drag
--  |  MouseRightDown   !Point !Modifiers -- ^ Mouse right button goes down
--  |  MouseRightUp     !Point !Modifiers -- ^ Mouse right  button goes up
--  |  MouseRightDClick !Point !Modifiers -- ^ Mouse right button double click
--  |  MouseRightDrag   !Point !Modifiers -- ^ Mouse right button drag (unsupported on most platforms)
--  |  MouseMiddleDown  !Point !Modifiers -- ^ Mouse middle button goes down
--  |  MouseMiddleUp    !Point !Modifiers -- ^ Mouse middle  button goes up
--  |  MouseMiddleDClick !Point !Modifiers -- ^ Mouse middle button double click
--  |  MouseMiddleDrag  !Point !Modifiers -- ^ Mouse middle button drag (unsupported on most platforms)
--  |  MouseWheel !Bool !Point !Modifiers -- ^ Mouse wheel rotation. (Bool is True for a downward rotation)
--  deriving (Eq) -- ,Show)

--- code in src/common/event.cpp

data EvtHandlerClass a = EvtHandlerClass {
     _evtHandlerAddPendingEvent :: Event -> IO ()
   , _evtHandlerBind :: EventType -> (Event -> IO ()) -> Id -> Id -> IO ()
   , _evtGetHandler :: EventType -> Id -> Id -> IO (Maybe (Event -> IO ()))
     -- not sure what this object user data thing is useful for
   --, _evtHandlerBind :: forall b c. EventTag -> (Event b -> IO ()) -> Id -> Id -> Object b -> IO ()
   -- weird functor matching functionality 
   --, _evtHandlerUnbind :: EventType -> (Event -> IO ()) -> Id -> Id -> IO ()
   , _evtHandlerUnBind :: EventType -> Id -> Id -> IO ()
   , _evtHandlerDeletePendingEvents :: IO ()
   , _evtHandlerGetEvtHandlerEnabled :: IO Bool
   , _evtHandlerGetNextHandler :: IO EvtHandler
   , _evtHandlerGetPreviousHandler :: IO EvtHandler
   , _evtHandlerIsUnlinked :: IO Bool
   , _evtHandlerProcessEvent :: Event -> IO Bool
   , _evtHandlerProcessEventLocally :: Event -> IO Bool
   , _evtHandlerProcessPendingEvents :: IO ()
   , _evtHandlerQueueEvent :: Event -> IO ()
   
   , _evtHandlerSetEvtHandlerEnabled :: Bool -> IO ()
   , _evtHandlerSetNextHandler :: EvtHandler -> IO ()
   , _evtHandlerSetPreviousHandler :: EvtHandler -> IO ()
   , _evtHandlerTryAfter :: Event -> IO ()
   , _evtHandlerTryBefore :: Event -> IO () 
   , _evtHandlerTryThis :: Event -> IO ()
   , _evtHandlerUnlink :: IO ()
   , _evtHandlerTail :: Record a
}
DefineSubClass(EvtHandler,Object,EvtHandlerClass,evtHandlerTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(EvtHandlerClass,evtHandlerTc,"EvtHandler")
#endif

evtHandler_Methods = unRecord . get_Object_Tail 
evtHandlerProcessEventLocally = _evtHandlerProcessEventLocally . evtHandler_Methods
evtHandlerBind = _evtHandlerBind . evtHandler_Methods
evtHandlerProcessEvent = _evtHandlerProcessEvent . evtHandler_Methods
evtHandlerGetHandler = _evtGetHandler . evtHandler_Methods
evtHandlerUnBind = _evtHandlerUnBind . evtHandler_Methods
-- static functions
--, _evtHandlerRemoveFilter(eventFilter* filter
--_evtHandlerAddFilter(eventFilter* filter


type DynamicEventTable = [DynamicEventTableEntry]

data DynamicEventTableEntry = DynamicEventTableEntry {
    dynamicEventTableEntryEventType :: EventType
   ,dynamicEventTableEntryId :: Id
   ,dynamicEventTableEntryLastId :: Id
   ,dynamicEventTableEntryFunc :: Event -> IO ()
   ,dynamicEventTableEntryUserData :: Object
}