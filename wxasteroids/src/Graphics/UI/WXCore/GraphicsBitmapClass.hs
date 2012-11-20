{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.GraphicsBitmapClass where

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
#ifdef __UHC__
import Language.UHC.JS.HTML5.HTMLImageElement
import Language.UHC.JS.HTML5.Types
#else
data HTMLImageElement
#endif

data GraphicsBitmapClass t = GraphicsBitmapClass {
     _graphicsBitmapGetNativeBitmap :: IO HTMLImageElement
   , _graphicsBitmapTail :: Record t   
}

DefineSubClass(GraphicsBitmap,GraphicsObject,GraphicsBitmapClass,graphicsBitmapTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(GraphicsBitmapClass,graphicsBitmapTc,"GraphicsBitmap")
#endif

graphicsBitmap_Methods = unRecord . get_GraphicsObject_Tail

graphicsBitmapGetNativeBitmap = _graphicsBitmapGetNativeBitmap . graphicsBitmap_Methods
