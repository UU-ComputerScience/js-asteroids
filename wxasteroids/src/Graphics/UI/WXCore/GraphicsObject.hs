module Graphics.UI.WXCore.GraphicsObject (
     module Graphics.UI.WXCore.GraphicsObjectClass
    ,graphicsObject
) where

import LightOO
import Graphics.UI.WXCore.GraphicsObjectClass

graphicsObject = 
   (graphicsObject' `extends` object) noOverride set_Object_Tail
   where
   graphicsObject' tail super self = 
      return GraphicsObjectClass {
         _graphicsObjectTail = tail
      }