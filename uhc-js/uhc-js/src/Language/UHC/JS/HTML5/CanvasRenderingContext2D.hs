module Language.UHC.JS.HTML5.CanvasRenderingContext2D where

import Language.UHC.JS.HTML5.Types
import Language.UHC.JS.Prelude
import Language.UHC.JS.Marshal
import Language.UHC.JS.Types

--void arc(
--  in float x,
--  in float y,
--  in float radius,
--  in float startAngle,
--  in float endAngle,
--  in boolean anticlockwise [optional]
--);
foreign import js "%1.arc(%*)"
   _arc :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> Double -> JSBool -> IO ()

--void arcTo(
--  in float x1,
--  in float y1,
--  in float x2,
--  in float y2,
--  in float radius
--);
foreign import js "%1.arcTo(%*)"
   _arcTo :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> Double -> IO ()

foreign import js "%1.beginPath()"
   beginPath :: CanvasRenderingContext2D -> IO ()

--void bezierCurveTo(
--  in float cp1x,
--  in float cp1y,
--  in float cp2x,
--  in float cp2y,
--  in float x,
--  in float y
--);
foreign import js "%1.bezierCurveTo(%*)"
   _bezierCurveTo :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

--void clearRect(
--  in float x,
--  in float y,
--  in float width,
--  in float height
--);
foreign import js "%1.clearRect(%*)"
   clearRect :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> IO ()

foreign import js "%1.clip()"
   clip :: CanvasRenderingContext2D -> IO ()

foreign import js "%1.closePath()"
   closePath :: CanvasRenderingContext2D -> IO ()

-- image data, tbi

-- gradients, tbi

foreign import js "%1.drawCustomFocusRing(%*)"
   _drawCustomFocusRing :: CanvasRenderingContext2D -> Element a -> IO JSBool

foreign import js "%1.drawSystemFocusRing(%*)"
   _drawSystemFocusRing :: CanvasRenderingContext2D -> Element a -> IO ()

-- draw image, tbi

foreign import js "%1.drawImage(%*)"
   drawImage :: CanvasRenderingContext2D -> HTMLImageElement -> Double -> Double -> Double -> Double -> IO ()

foreign import js "%1.fill()"
   fill :: CanvasRenderingContext2D -> IO ()


fillRect :: Double -> Double -> Double -> Double -> CanvasRenderingContext2D -> IO ()
fillRect a b c d dc = _fillRect dc a b c d

--void fillRect(
--  in float x,
--  in float y,
--  in float width,
--  in float height
--);
foreign import js "%1.fillRect(%*)"
   _fillRect :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> IO ()

--void fillText(
--  in DOMString text,
--  in float x,
--  in float y,
--  in float maxWidth [optional]
--);
foreign import js "%1.fillText(%*)"
   _fillText :: CanvasRenderingContext2D -> JSString -> Double -> Double -> Double -> IO ()

--boolean isPointInPath(
--  in float x,
--  in float y
--);
foreign import js "%1.isPointInPath(%*)"
   _isPointInPath :: CanvasRenderingContext2D -> Double -> Double -> IO JSBool


lineTo :: Double -> Double -> CanvasRenderingContext2D -> IO ()
lineTo a b dc = _lineTo dc a b

--void lineTo(
--  in float x,
--  in float y
--);
foreign import js "%1.lineTo(%*)"
   _lineTo :: CanvasRenderingContext2D -> Double -> Double -> IO ()

--nsIDOMTextMetrics measureText(
--  in DOMString text
--);
foreign import js "%1.measureText(%*)"
   _measureText :: CanvasRenderingContext2D -> JSString -> IO (JSObject a)


moveTo :: Double -> Double -> CanvasRenderingContext2D -> IO ()
moveTo a b dc = _moveTo dc a b 
--void moveTo(
--  in float x,
--  in float y
--);
foreign import js "%1.moveTo(%*)"
   _moveTo :: CanvasRenderingContext2D -> Double -> Double -> IO ()

-- putImageData

--void quadraticCurveTo(
--  in float cpx,
--  in float cpy,
--  in float x,
--  in float y
--);
foreign import js "%1.quadraticCurveTo(%*)"
   _quadraticCurveTo :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> IO ()

--void rect(
--  in float x,
--  in float y,
--  in float w,
--  in float h
--);
foreign import js "%1.rect(%*)"
   _rect :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> IO ()

foreign import js "%1.restore()"
   restore :: CanvasRenderingContext2D -> IO ()

rotate a dc = _rotate dc a

--void rotate(
--  in float angle
--);
foreign import js "%1.rotate(%*)"
   _rotate :: CanvasRenderingContext2D -> Double -> IO ()

foreign import js "%1.save()"
   save :: CanvasRenderingContext2D -> IO ()

--void scale(
--  in float x,
--  in float y
--);
foreign import js "%1.scale(%*)"
   _scale :: CanvasRenderingContext2D -> Double -> Double -> IO ()

foreign import js "%1.scrollPathIntoView()"
   _scrollPathIntoView :: CanvasRenderingContext2D -> IO ()

--void setTransform(
--  in float m11,
--  in float m12,
--  in float m21,
--  in float m22,
--  in float dx,
--  in float dy
--);
foreign import js "%1.setTransform(%*)"
   _setTransform :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

foreign import js "%1.stroke()"
   stroke :: CanvasRenderingContext2D -> IO ()

--void strokeRect(
--  in float x,
--  in float y,
--  in float w,
--  in float h
--);
foreign import js "%1.strokeRect(%*)"
   _strokeRect :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> IO ()

--void strokeText(
--  in DOMString text,
--  in float x,
--  in float y,
--  in float maxWidth [optional]
--);
foreign import js "%1.strokeText(%*)"
   _strokeText :: CanvasRenderingContext2D -> JSString -> Double -> Double -> Double -> IO ()

--void transform(
--  in float m11,
--  in float m12,
--  in float m21,
--  in float m22,
--  in float dx,
--  in float dy
--);
foreign import js "%1.setTransform(%*)"
   setTransform :: CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

translate a b dc = _translate dc a b

--void translate(
--  in float x,
--  in float y
--);
foreign import js "%1.translate(%*)"
   _translate :: CanvasRenderingContext2D -> Double -> Double -> IO ()

-- properties

type Color = String
type Font = String

setFillStyle :: Color -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setFillStyle c dc = setAttr "fillStyle" (toJS c :: JSString) dc

setFont :: Font -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setFont c dc = setAttr "font" (toJS c :: JSString) dc

setGlobalAlpha :: Double -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setGlobalAlpha a dc = setAttr "globalAlpha" a dc

data GlobalCompositeOperation = 
   SourceATop | SourceIn | SourceOut | SourceOver | DestinationATop | DestinationIn | DestinationOut | DestinationOver
   | Lighter | Xor

instance Show GlobalCompositeOperation where
   show x = 
      case x of
         SourceATop -> "source-a-top"
         SourceIn -> "source-in"
         SourceOut -> "source-out"
         SourceOver -> "source-over"
         DestinationOver -> "destination-over"
         DestinationOut -> "destination-out"
         DestinationIn -> "destination-in"
         DestinationATop -> "destination-atop"
         Lighter -> "lighter"
         Xor -> "xor"

setGlobalCompositeOperation :: GlobalCompositeOperation -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setGlobalCompositeOperation a dc = setAttr "globalCompositeOperation" (toJS (show a) :: JSString) dc

setLineCap :: String -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setLineCap a dc = setAttr "setLineCap" (toJS a :: JSString) dc

setLineJoin :: String -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setLineJoin a dc = setAttr "lineJoin" (toJS a :: JSString) dc

setLineWidth :: Double -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setLineWidth a dc = setAttr "lineWidth" a dc

setMiterLimit :: Double -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setMiterLimit a dc = setAttr "miterLimit" a dc

setShadowBlur :: Double -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setShadowBlur a dc = setAttr "shadowBlur" a dc

setShadowColor :: Color -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setShadowColor a dc = setAttr "shadowColor" (toJS a :: JSString) dc

setShadowOffsetX :: Double -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setShadowOffsetX a dc = setAttr "shadowOffsetX" a dc

setShadowOffsetY :: Double -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setShadowOffsetY a dc = setAttr "shadowOffsetY" a dc

setStrokeStyle :: String -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setStrokeStyle a dc = setAttr "strokeStyle"  a dc

setTextAlign :: String -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setTextAlign a dc = setAttr "textAlign" a dc

setTextBaseLine :: String -> CanvasRenderingContext2D -> IO CanvasRenderingContext2D
setTextBaseLine a dc = setAttr "textBaseLine" a dc
