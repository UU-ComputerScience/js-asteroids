{-# LANGUAGE CPP #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.GraphicsContext (
    module Graphics.UI.WXCore.GraphicsContextClass
   ,canvasGraphicsContext
) where

import LightOO
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.GraphicsContextClass
import Graphics.UI.WXCore.GraphicsBitmapClass
import Graphics.UI.WXCore.GraphicsObject
import Data.IORef
import System.IO.Unsafe
#ifdef __UHC__
import Language.UHC.JS.HTML5.CanvasRenderingContext2D
#else
fillRect = undefined
drawImage = undefined
setTransform = undefined
clearRect = undefined
#endif
{-
wxMacCoreGraphicsContext::wxMacCoreGraphicsContext( wxGraphicsRenderer* renderer, wxWindow* window ): wxGraphicsContext(renderer)
{
    Init();

    m_enableOffset = true;
    wxSize sz = window->GetSize();
    m_width = sz.x;
    m_height = sz.y;

#if wxOSX_USE_COCOA_OR_IPHONE
    m_view = window->GetHandle();

#if wxOSX_USE_COCOA
    if ( ! window->GetPeer()->IsFlipped() )
    {
        m_windowTransform = CGAffineTransformMakeTranslation( 0 , m_height );
        m_windowTransform = CGAffineTransformScale( m_windowTransform , 1 , -1 );
    }
    else
#endif
    {
        m_windowTransform = CGAffineTransformIdentity;
    }
#else
    int originX , originY;
    originX = originY = 0;
    Rect bounds = { 0,0,0,0 };
    m_windowRef = (WindowRef) window->MacGetTopLevelWindowRef();
    window->MacWindowToRootWindow( &originX , &originY );
    GetWindowBounds( m_windowRef, kWindowContentRgn, &bounds );
    m_windowTransform = CGAffineTransformMakeTranslation( 0 , bounds.bottom - bounds.top );
    m_windowTransform = CGAffineTransformScale( m_windowTransform , 1 , -1 );
    m_windowTransform = CGAffineTransformTranslate( m_windowTransform, originX, originY ) ;
#endif
}
-}

canvasGraphicsContext dc =
   (canvasGraphicsContext' `extends` graphicsObject) noOverride set_GraphicsObject_Tail
   where 
   canvasGraphicsContext' tail super self = do
      return GraphicsContextClass {
        _graphicsContextBeginLayer = error "_graphicsContextBeginLayer not implemented"
      , _graphicsContextClip = error "_graphicsContextClip not implemented"
      , _graphicsContextConcatTransform = error "_graphicsContextConcatTransform not implemented"
      , _graphicsContextCreateBrush = error "_graphicsContextCreateBrush not implemented"
      , _graphicsContextCreateFont = error "_graphicsContextCreateFont not implemented"
      , _graphicsContextDisableOffset = error "_graphicsContextDisableOffset not implemented"
      , _graphicsContextDrawEllipse = error "_graphicsContextDrawEllipse not implemented"
      , _graphicsContextDrawLines = error "_graphicsContextDrawLines not implemented"
      , _graphicsContextDrawPath = error "_graphicsContextDrawPath not implemented"
      , _graphicsContextDrawRectangle = \x y w h -> do
        dc # fillRect x y w h
      , _graphicsContextDrawRoundedRectangle = error "_graphicsContextDrawRoundedRectangle not implemented"
      , _graphicsContextDrawText = error "_graphicsContextDrawText not implemented"
      , _graphicsContextDrawBitmap = \bm x y w h -> do
        img <- bm # graphicsBitmapGetNativeBitmap
        (dc # drawImage) img x y w h
      , _graphicsContextEnableOffset = error "_graphicsContextEnableOffset not implemented"
      , _graphicsContextEndLayer = error "_graphicsContextEndLayer not implemented"
      , _graphicsContextFillPath = error "_graphicsContextFillPath not implemented"
      , _graphicsContextGetAntialiasMode = error "_graphicsContextGetAntialiasMode not implemented"
      , _graphicsContextGetCompositionMode = error "_graphicsContextGetCompositionMode not implemented"
      , _graphicsContextGetInterpolationQuality = error "_graphicsContextGetInterpolationQuality not implemented"
      , _graphicsContextGetTransform = error "_graphicsContextGetTransform not implemented"
      , _graphicsContextResetClip = error "_graphicsContextResetClip not implemented"
      , _graphicsContextRotate = error "_graphicsContextRotate not implemented"
      , _graphicsContextScale = error "_graphicsContextScale not implemented"
      , _graphicsContextSetAntialiasMode = error "_graphicsContextSetAntialiasMode not implemented"
      , _graphicsContextSetBrush = error "_graphicsContextSetBrush not implemented"
      , _graphicsContextSetCompositionMode = error "_graphicsContextSetCompositionMode not implemented"
      , _graphicsContextSetFont = error "_graphicsContextSetFont not implemented"
      , _graphicsContextSetInterpolationQuality = error "_graphicsContextSetInterpolationQuality not implemented"
      , _graphicsContextSetPen = error "_graphicsContextSetPen not implemented"
      , _graphicsContextSetTransform = error "_graphicsContextSetTransform not implemented"
      , _graphicsContextStrokeLine = error "_graphicsContextStrokeLine not implemented"
      , _graphicsContextStrokePath = error "_graphicsContextStrokePath not implemented"
      , _graphicsContextTranslate = error "_graphicsContextTranslate not implemented"
      , _graphicsContextClear = \rect -> do
        setTransform dc 1 0 0 1 0 0
        clearRect dc (fromIntegral $ rectLeft rect) 
                     (fromIntegral $ rectTop rect) 
                     (fromIntegral $ rectWidth rect) 
                     (fromIntegral $ rectHeight rect)
      , _graphicsContextTail = tail
   }