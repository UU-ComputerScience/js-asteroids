{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.GraphicsRendererClass where

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else
#include "LightOO.h"
#endif

import LightOO
import Data.Typeable
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.WindowClass
import Graphics.UI.WXCore.GraphicsContextClass

data GraphicsRendererClass a = GraphicsRendererClass {
    _graphicsRendererCreateContextFromWindow :: Window -> IO GraphicsContext
   ,_graphicsRendererTail :: Record a
}
DefineSubClass(GraphicsRenderer,Object,GraphicsRendererClass,graphicsRendererTail,,,,1,)

graphicsRendererCreateContextFromWindow = _graphicsRendererCreateContextFromWindow . unRecord . get_Object_Tail

#ifdef __UHC__
INSTANCE_TYPEABLE1(GraphicsRendererClass,graphicsRendererTc,"GraphicsRenderer")
#endif

