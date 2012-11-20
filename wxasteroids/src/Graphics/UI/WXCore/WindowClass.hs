{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, FlexibleContexts #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.WindowClass where

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else
#include "LightOO.h"
#endif

import LightOO
import Data.Maybe
import Data.Typeable
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.EvtHandlerClass
import Graphics.UI.WXCore.GraphicsContextClass
import Graphics.UI.WXCore.Events (EventKey)

data WindowClass t = WindowClass {

     _windowGetId :: IO Id
   , _windowSetId :: Id -> IO ()
   , _windowGetStyle :: IO Style
   , _windowGetRect :: IO Rect
   , _windowGetPosition :: IO Point
   , _windowGetSize :: IO Size
   , _windowSetSize :: Rect -> IO ()

   -- child management
   , _windowAddChild :: Window -> IO ()
   , _windowDestroyChildren :: IO ()
   , _windowFindWindow :: Id -> IO (Maybe Window)
   , _windowFindWindowByName :: String -> IO (Maybe Window)
   , _windowGetChildren :: IO [Window]
   , _windowRemoveChild :: Window -> IO ()

   -- sibling and parent management functions
   , _windowGetGrandParent :: IO (Maybe Window)
   , _windowGetNextSibling :: IO (Maybe Window)
   , _windowGetParent :: IO (Maybe Window)
   , _windowGetPrevSibling :: IO (Maybe Window)
   , _windowIsDescendant :: Window -> IO Bool
   , _windowReparent :: Window -> IO Bool

   -- window deletion functions
   , _windowClose :: Bool -> IO Bool
   , _windowDestroy :: IO Bool
   , _windowIsBeingDeleted :: IO Bool

   -- window status functions
   , _windowHide :: IO Bool
   --, _windowHideWithEffect
   , _windowIsEnabled :: IO Bool
   -- overloaded functions not supported
   , _windowIsExposed :: Int -> Int -> Int -> Int -> IO Bool
   , _windowIsShown :: IO Bool
   , _windowIsShownOnScreen :: IO Bool
   , _windowDisable :: IO Bool
   , _windowEnable :: Bool -> IO Bool
   , _windowShow :: IO Bool
   , _windowRefresh :: Bool -> IO ()

   , _windowGetClientSize :: IO Size
   --, _windowShowWithEffect

   --, windowDestroy :: Window a -> IO Bool

   --, windowGetId :: Window a -> IO Int

   --, windowGetParent :: Window a -> IO (Window ())
    
   --, windowGetPosition :: Window a -> IO Point

   --, windowHide :: Window a -> IO Bool
    
   --, windowIsShown :: Window a -> IO Bool
    
   --, windowRaise :: Window a -> IO ()

   --, windowSetClientSize :: Window a -> Size -> IO ()

   --, windowSetForegroundColour :: Window a -> Color -> IO Int
    
   --, windowSetId :: Window a -> Id -> IO ()

   --, windowSetName :: Window a -> String -> IO ()

   , _windowGetLabel :: IO String

   -- non window method, but I don't wan to bother with defining a new class
   , _windowGetGraphicsContext :: IO GraphicsContext
   , _windowOnKeyChar :: (EventKey -> IO ()) -> IO ()
   , _windowGetOnKeyChar :: IO (EventKey -> IO ())

   , _windowTail :: Record t
}
DefineSubClass(Window,EvtHandler,WindowClass,windowTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(WindowClass,windowTc,"Window")
#endif

window_Methods = unRecord . get_EvtHandler_Tail

windowAddChild = _windowAddChild . window_Methods
windowGetId = _windowGetId . window_Methods
windowSetId =  _windowSetId . window_Methods
windowGetChildren = _windowGetChildren . window_Methods
windowFindWindow = _windowFindWindow . window_Methods
windowGetParent = _windowGetParent . window_Methods
windowGetNextSibling = _windowGetNextSibling . window_Methods
windowDestroy = _windowDestroy . window_Methods
windowDestroyChildren = _windowDestroyChildren . window_Methods
windowRemoveChild = _windowRemoveChild . window_Methods
windowIsDescendant = _windowIsDescendant . window_Methods
windowReparent = _windowReparent . window_Methods
windowShow = _windowShow . window_Methods
windowGetStyle = _windowGetStyle . window_Methods
windowGetPosition = _windowGetPosition . window_Methods
windowGetSize = _windowGetSize . window_Methods
windowClose = _windowClose . window_Methods
windowGetGraphicsContext = _windowGetGraphicsContext . window_Methods

windowGetEffectiveMinSize = error "windowGetEffectiveMinSize not implemented"
windowMove = error "windowMove not implemented"
windowGetClientSize = _windowGetClientSize . window_Methods
windowSetClientSize = error "windowSetClientSize not implemented"
windowGetVirtualSize = error "windowGetVirtualSize not implemented"
windowSetVirtualSize = error "windowSetVirtualSize not implemented"
windowGetBackgroundColour = error "windowGetBackgroundColour not implemented"
windowSetBackgroundColour = error "windowSetBackgroundColour not implemented"
windowGetForegroundColour = error "windowGetForegroundColour not implemented"
windowSetForegroundColour = error "windowSetForegroundColour not implemented"
windowIsShown = error "windowIsShown not implemented"
windowRaise = error "windowRaise not implemented"
windowHide = error "windowHide not implemented"
windowRefresh = _windowRefresh . window_Methods
windowGetRect = _windowGetRect .window_Methods

windowSetSize = _windowSetSize . window_Methods
windowIsEnabled = error "windowIsEnabled not implemented"
windowEnable = error "windowEnable not implemented"
windowDisable = error "windowDisable not implemented"
windowGetLabel = error "windowGetLabel not implemented"
windowSetLabel = error "windowSetLabel not implemented"
windowChildren = error "windowChildren not implemented"
windowReFit = error "windowReFit not implemented"
windowReFitMinimal = error "windowReFitMinimal not implemented"
windowGetRootParent = error "windowGetRootParent not implemented"
windowGetFrameParent = error "windowGetFrameParent not implemented"
windowGetWindowStyleFlag = error "windowGetWindowStyleFlag not implemented"
windowSetWindowStyleFlag = error "windowSetWindowStyleFlag not implemented"
windowGetToolTip = error "windowGetToolTip not implemented"
windowSetToolTip = error "windowSetToolTip not implemented"
windowGetOnMouse = error "windowGetOnMouse not implemented"
windowOnMouse = error "windowOnMouse not implemented"
windowGetOnKeyChar = _windowGetOnKeyChar . window_Methods
windowOnKeyChar = _windowOnKeyChar . window_Methods
windowGetOnClose = error "windowGetOnClose not implemented"
windowOnClose = error "windowOnClose not implemented"
windowGetOnIdle = error "windowGetOnIdle not implemented"
windowOnIdle = error "windowOnIdle not implemented"
windowGetOnSize = error "windowGetOnSize not implemented"
windowOnSize = error "windowOnSize not implemented"
windowGetOnFocus = error "windowGetOnFocus not implemented"
windowOnFocus = error "windowOnFocus not implemented"
windowGetOnActivate = error "windowGetOnActivate not implemented"
windowOnActivate = error "windowOnActivate not implemented"