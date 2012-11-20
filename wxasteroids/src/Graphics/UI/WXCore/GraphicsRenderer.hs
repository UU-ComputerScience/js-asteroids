module Graphics.UI.WXCore.GraphicsRenderer (
    module Graphics.UI.WXCore.GraphicsRendererClass
   ,graphicsRenderer
   ,graphicsRendererGetDefaultRenderer
) where

import LightOO
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.GraphicsRendererClass
import Graphics.UI.WXCore.GraphicsContext
import Data.IORef
import System.IO.Unsafe

graphicsRenderer =
   (graphicsRenderer' `extends` object) noOverride set_Object_Tail
   where
   graphicsRenderer' tail super self =
      return GraphicsRendererClass {
      {-
      wxGraphicsContext * wxMacCoreGraphicsRenderer::CreateContext( wxWindow* window )
      {
         return new wxMacCoreGraphicsContext(this, window );
      }
      -}
           _graphicsRendererCreateContextFromWindow = \w -> do
              return undefined
               --let gr :: GraphicsRenderer
               --    gr = upcast self
               --new $ canvasGraphicsContext w gr
         , _graphicsRendererTail = tail
      }

defaultRendererRef :: IORef (Maybe GraphicsRenderer)
defaultRendererRef = unsafePerformIO (newIORef Nothing)

graphicsRendererGetDefaultRenderer :: IO GraphicsRenderer
graphicsRendererGetDefaultRenderer =
   singleton defaultRendererRef graphicsRenderer