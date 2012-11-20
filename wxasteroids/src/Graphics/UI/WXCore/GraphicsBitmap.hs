{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.GraphicsBitmap (
     module Graphics.UI.WXCore.GraphicsBitmapClass
   , bitmap
) where

import LightOO
import Data.Typeable
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.GraphicsBitmapClass
import Graphics.UI.WXCore.GraphicsObject
#ifdef __UHC__
import Language.UHC.JS.HTML5.HTMLImageElement
#else
newImage = undefined
src = undefined
#endif

bitmap source = 
   (bitmap' `extends` graphicsObject) noOverride set_GraphicsObject_Tail
   where 
   bitmap' tail super self = do
      return GraphicsBitmapClass {
           _graphicsBitmapGetNativeBitmap = do
            img <- newImage 
            src img source
            return img
         , _graphicsBitmapTail = tail
      }