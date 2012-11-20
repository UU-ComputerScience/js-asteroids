{-# LANGUAGE CPP #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore (
#ifdef __UHC__
    module Graphics.UI.WXCore.WebWindow
#else
    module Graphics.UI.WXCore.Window
#endif

   ,module Graphics.UI.WXCore.Types
   ,module Graphics.UI.WXCore.GraphicsRenderer
   ,module Graphics.UI.WXCore.GraphicsContext
   ,module Graphics.UI.WXCore.GraphicsObject
   ,module Graphics.UI.WXCore.GraphicsBitmap

   ,module Graphics.UI.WXCore.EvtHandler

   ,module Graphics.UI.WXCore.Event
   ,module Graphics.UI.WXCore.Events

   ,module Graphics.UI.WXCore.Timer

   --,module Graphics.UI.WXCore.TopLevelWindow
   --,module Graphics.UI.WXCore.Frame
   ,run
) where

#ifdef __UHC__
import Graphics.UI.WXCore.WebWindow
import qualified Language.UHC.JS.HTML5.Window as W
import Language.UHC.JS.Prelude
#else
import Graphics.UI.WXCore.Window
#endif

import Graphics.UI.WXCore.GraphicsRenderer
import Graphics.UI.WXCore.GraphicsContext
import Graphics.UI.WXCore.GraphicsObject
import Graphics.UI.WXCore.GraphicsBitmap
import Graphics.UI.WXCore.Types

import Graphics.UI.WXCore.EvtHandler

import Graphics.UI.WXCore.Event
import Graphics.UI.WXCore.Events

import Graphics.UI.WXCore.Timer hiding (timer)

--import Graphics.UI.WXCore.TopLevelWindow
--import Graphics.UI.WXCore.Frame hiding (frame)

{-
-- | Start the event loop. Takes an initialisation action as argument.
-- Except for 'run', the functions in the WXH library can only be called
-- from this intialisation action or from event handlers, or otherwise bad
-- things will happen :-)
run :: IO a -> IO ()
run init
  = do appOnInit (do wxcAppInitAllImageHandlers
                     init
                     return ())
       performGC
       performGC
-}
run :: IO a -> IO ()
run init = do
   appOnInit (do init; return ())

#ifdef __UHC__
appOnInit f = do
   w <- W.window
   cb <- wrapFunc f
   setAttr_ "onload" cb w
   return ()
#else
appOnInit = undefined
#endif