module Language.UHC.JS.HTML5.HTMLImageElement where

import Language.UHC.JS.Prelude
import Language.UHC.JS.Marshal
import Language.UHC.JS.HTML5.Types

foreign import js "new Image()"
   newImage :: IO HTMLImageElement

src :: HTMLImageElement -> String -> IO ()
src e s = setAttr_ "src" (str s) e

width :: HTMLImageElement -> Double -> IO ()
width e d = setAttr_ "width" d e

getWidth :: HTMLImageElement -> IO Double
getWidth e = getAttr "width" e 

height :: HTMLImageElement -> Double -> IO ()
height e d = setAttr_ "height" d e

getHeight :: HTMLImageElement -> IO Double
getHeight e = getAttr "height" e 