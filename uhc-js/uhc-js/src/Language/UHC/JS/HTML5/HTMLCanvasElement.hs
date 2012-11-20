{-
interface HTMLCanvasElement : HTMLElement {
           attribute unsigned long width;
           attribute unsigned long height;

  DOMString toDataURL(optional DOMString type, any... args);
  void toBlob(FileCallback? _callback, optional DOMString type, any... args);

  object? getContext(DOMString contextId, any... args);
};
-}
module Language.UHC.JS.HTML5.HTMLCanvasElement where

import Language.UHC.JS.Types
import Language.UHC.JS.Marshal
import Language.UHC.JS.HTML5.Types
import Language.UHC.JS.Prelude

foreign import js "%1.width"
   getWidth :: HTMLCanvasElement -> IO Integer

width :: HTMLCanvasElement -> Int -> IO ()
width e w = setAttr_ "width" w e

foreign import js "%1.height"
   getHeight :: HTMLCanvasElement -> IO Integer 

height :: HTMLCanvasElement -> Int -> IO ()
height e w = setAttr_ "height" w e

foreign import js "%1.toDataURL(%*)"
   toDataURL :: HTMLCanvasElement -> JSArray JSString -> IO JSString

foreign import js "%1.getContext(%*)"
  getContext :: HTMLCanvasElement -> JSString -> IO JSObject

get2DContext :: HTMLCanvasElement -> IO (Maybe CanvasRenderingContext2D)
get2DContext e = do
  let d :: JSString
      d = toJS "2d"
  o <- getContext e d
  return $ cast o
