{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.GraphicsObjectClass where

#ifdef __UHC__
#include "Typeable.h"
#include "LightOOUHC.h"
#else
#include "LightOO.h"
#endif

import LightOO
import Data.Typeable
import Graphics.UI.WXCore.Types
--import Graphics.UI.WXCore.GraphicsRendererClass

data GraphicsObjectClass a = GraphicsObjectClass {
   -- commented out because it causes a cycle in modules deps
    -- _graphicsObjectGetRenderer :: IO GraphicsRenderer
   _graphicsObjectTail :: Record a
}
DefineSubClass(GraphicsObject,Object,GraphicsObjectClass,graphicsObjectTail,,,,1,)

#ifdef __UHC__
INSTANCE_TYPEABLE1(GraphicsObjectClass,graphicsObjectTc,"GraphicsObject")
#endif

graphicsObject_Methods = unRecord . get_Object_Tail