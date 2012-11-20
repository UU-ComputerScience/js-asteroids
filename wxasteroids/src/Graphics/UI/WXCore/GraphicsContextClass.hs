{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.GraphicsContextClass where

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else
#include "LightOO.h"
#endif

import LightOO
import Data.Typeable
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.GraphicsObjectClass
import Graphics.UI.WXCore.GraphicsBitmapClass

data GraphicsMatrix
data GraphicsFont
data GraphicsBrush 
data Brush
data GraphicsPen
data GraphicsPath

type PolygonFillMode = Int
type AntialiasMode = Int
type CompositionMode = Int
type Colour = Int
type InterpolationQuality = Int

type Point2D = (Double,Double)

data GraphicsContextClass t = GraphicsContextClass {
     _graphicsContextBeginLayer :: Double -> IO ()
   --, _graphicsContextClip(const Region& region
   , _graphicsContextClip :: Double -> Double -> Double -> Double -> IO ()
   , _graphicsContextConcatTransform :: GraphicsMatrix -> IO ()
   --, _graphicsContextCreate(const EnhMetaFileDC& metaFileDC
   --, _graphicsContextCreate(const MemoryDC& memoryDC
   --, _graphicsContextCreate(const PrinterDC& printerDC
   --, _graphicsContextCreate(const WindowDC& windowDC
   --, _graphicsContextCreate(Image& image
   --, _graphicsContextCreate(Window* window
   --, _graphicsContextCreateBitmap :: Bitmap -> IO ()
   --, _graphicsContextCreateBitmapFromImage(const Image& image
   , _graphicsContextCreateBrush :: Brush -> IO GraphicsBrush
   --, _graphicsContextCreateFont :: Font -> Colour -> IO GraphicsFont
   , _graphicsContextCreateFont :: Double -> String -> Int -> Colour -> IO GraphicsFont 
   --, _graphicsContextCreateFromNativeWindow(void* window
   --, _graphicsContextCreateLinearGradientBrush(Double x1, Double y1, Double x2, Double y2, const Colour& c1, const Colour& c2) const, CreateLinearGradientBrush(Double x1, Double y1, Double x2, Double y2, const GraphicsGradientStops& stops) const, CreateMatrix(Double a, Double b, Double c, Double d, Double tx, Double ty) const, CreatePath() const, CreatePen(const Pen& pen) const, CreateRadialGradientBrush(Double xo, Double yo, Double xc, Double yc, Double radius, const Colour& oColor, const Colour& cColor) const, CreateRadialGradientBrush(Double xo, Double yo, Double xc, Double yc, Double radius, const GraphicsGradientStops& stops
   --, _graphicsContextCreateSubBitmap(const GraphicsBitmap& bitmap, Double x, Double y, Double w, Double h
   , _graphicsContextDisableOffset :: IO ()
   , _graphicsContextDrawBitmap :: GraphicsBitmap -> Double -> Double -> Double -> Double -> IO () 
   --, _graphicsContextDrawBitmap(const GraphicsBitmap& bmp, Double x, Double y, Double w, Double h
   , _graphicsContextDrawEllipse :: Double -> Double -> Double -> Double -> IO ()
   --, _graphicsContextDrawIcon(const Icon& icon, Double x, Double y, Double w, Double h
   , _graphicsContextDrawLines :: [Point2D] -> PolygonFillMode -> IO ()
   , _graphicsContextDrawPath :: GraphicsPath -> PolygonFillMode -> IO ()
   , _graphicsContextDrawRectangle :: Double -> Double -> Double -> Double -> IO ()
   , _graphicsContextDrawRoundedRectangle :: Double -> Double -> Double -> Double -> Double -> IO ()
   , _graphicsContextDrawText :: String -> Double -> Double -> Double -> GraphicsBrush -> IO ()
   --, _graphicsContextDrawText(const String& str, Double x, Double y, const GraphicsBrush& backgroundBrush
   --, _graphicsContextDrawText(const String& str, Double x, Double y, Double angle
   --, _graphicsContextDrawText(const String& str, Double x, Double y, Double angle, const GraphicsBrush& backgroundBrush
   , _graphicsContextEnableOffset :: IO Bool
   , _graphicsContextEndLayer :: IO ()
   , _graphicsContextFillPath :: GraphicsPath -> PolygonFillMode -> IO ()
   , _graphicsContextGetAntialiasMode :: IO AntialiasMode
   , _graphicsContextGetCompositionMode :: IO CompositionMode
   , _graphicsContextGetInterpolationQuality :: IO InterpolationQuality
   --, _graphicsContextGetPartialTextExtents(const String& text, ArrayDouble& widths) const, GetTextExtent(const String& text, Double* width, Double* height, Double* descent, Double* externalLeading) const, 
   , _graphicsContextGetTransform :: IO GraphicsMatrix
   , _graphicsContextResetClip :: IO ()
   , _graphicsContextRotate :: Double -> IO ()
   , _graphicsContextScale :: Double -> Double -> IO ()
   , _graphicsContextSetAntialiasMode :: AntialiasMode -> IO ()
   --, _graphicsContextSetBrush :: Brush -> IO ()
   , _graphicsContextSetBrush :: GraphicsBrush -> IO ()
   , _graphicsContextSetCompositionMode :: CompositionMode -> IO ()
   , _graphicsContextSetFont :: GraphicsFont -> Colour -> IO () 
   --, _graphicsContextSetFont(const GraphicsFont& font
   , _graphicsContextSetInterpolationQuality :: InterpolationQuality -> IO ()
   , _graphicsContextSetPen :: GraphicsPen -> IO ()
   --, _graphicsContextSetPen(const Pen& pen
   , _graphicsContextSetTransform :: GraphicsMatrix -> IO ()
   , _graphicsContextStrokeLine :: Double -> Double -> Double -> Double -> IO () 
   --, _graphicsContextStrokeLines(size_t n, const Point2DDouble* beginPoints, const Point2DDouble* endPoints
   --, _graphicsContextStrokeLines(size_t n, const Point2DDouble* points
   , _graphicsContextStrokePath :: GraphicsPath -> IO ()
   , _graphicsContextTranslate :: Double -> Double -> IO ()

   -- not in the API..?
   , _graphicsContextClear :: Rect -> IO ()

   , _graphicsContextTail :: Record t
}
DefineSubClass(GraphicsContext,GraphicsObject,GraphicsContextClass,graphicsContextTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(GraphicsContextClass,graphicsContextTc,"GraphicsContext")
#endif

graphicsContext_Methods = unRecord . get_GraphicsObject_Tail

graphicsContextDrawRectangle = _graphicsContextDrawRectangle . graphicsContext_Methods
graphicsContextDrawBitmap = _graphicsContextDrawBitmap . graphicsContext_Methods
graphicsContextClear = _graphicsContextClear . graphicsContext_Methods