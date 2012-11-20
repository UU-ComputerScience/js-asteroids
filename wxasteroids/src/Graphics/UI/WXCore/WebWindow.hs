{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Graphics.UI.WXCore.WebWindow (
    module Graphics.UI.WXCore.WindowClass
   ,windowCreate
   ,windowTimerCreate
   ,windowOnPaint
   ,windowGetOnPaint
   ,window
) where

import LightOO
import Data.IORef
import Data.Maybe
import Data.Typeable hiding (cast)
import Control.Monad
import Graphics.UI.WXCore.WindowClass
import Graphics.UI.WXCore.GraphicsContext
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.EvtHandler
import Graphics.UI.WXCore.Timer
import Graphics.UI.WXCore.Event
import Graphics.UI.WXCore.PaintEvent
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.KeyEvent
import Language.UHC.JS.HTML5.HTMLDocument
import Language.UHC.JS.HTML5.HTMLCanvasElement
import qualified Language.UHC.JS.HTML5.Window as W
import Language.UHC.JS.HTML5.Types hiding (Window,Window_)
import Language.UHC.JS.HTML5.HTMLElement
import Language.UHC.JS.HTML5.Node
import Language.UHC.JS.HTML5.CSSStyleDeclaration
import Language.UHC.JS.Types
import Language.UHC.JS.JSRef
import Language.UHC.JS.Prelude
import Language.UHC.JS.Marshal

windowTimerCreate :: Window -> IO Timer
windowTimerCreate w = new $ timer (upcast w) idAny

windowCreate p id rect style = new $ window p id rect style  

window p id rect style tail self = do
   w <- window' p id (rectTopLeft rect) (rectSize rect) style tail self
   maybe (return ()) (\p -> p # windowAddChild $ w) p
   return w   

--window :: Maybe (Window a) -> Id -> Rect -> Style -> IO (t1 -> a) -> t1 -> IO (Window a)
window' p ident pos size style' = 
   (wrapper `extends` evthandler) noOverride set_EvtHandler_Tail

   where

   findWindow' self ident = do
      ident' <- self # windowGetId
      if ident == ident'
         then return $ Just self
         else recurseInChildren ident
      where
      recurseInChildren ident = do
         c <- self # windowGetChildren
         r <- filterM (\w -> w # windowGetId >>= \i -> return $ i == ident) c
         case r of
            []    -> foldM (\a w -> (w # windowFindWindow) ident >>= \r -> return $ a `mplus` r)
                           Nothing c 
            (x:_) -> return $ Just x

   withSiblings self f = do
      p <- self # windowGetParent
      case p of
         Nothing -> return Nothing
         Just p  -> do { c <- p # windowGetChildren
                       ; ident <- self # windowGetId
                       ; r <- partitionByWindowId ident c
                       ; f r
                       }

   wrapper tail super self = do
      childrenRef <- newIORef [] :: IO (IORef [Window])
      styleRef <- newIORef style'

      freshId <- if ident == wxID_ANY then idCreate else return ident
      identRef <- newIORef freshId
      posRef <- newIORef pos :: IO (IORef Point)

      gcRef <- newIORef Nothing
 
      let initialize = do
            d   <- document
            rep <- createElement d (str "div")

            let htmlDiv :: HTMLElement
                htmlDiv = maybe (error "not a div element") id (cast rep)

            parentEl <- getAttr "body" d
            parentEl # appendChild $ htmlDiv

            return htmlDiv

      htmlRep <- initialize

      let cssRef = newReadOnlyJSRef (getAttr "style" htmlRep)

      let gett p cssRef = do
            css <- readJSRef cssRef
            v <- getPropertyValue css (str p) 
            return $ parseInt (fromJust $ fromJS v)
          sett p cssRef (v :: Int) = do
            css <- readJSRef cssRef
            let v' = (show v) ++ "px"
            setProperty css (str p) (str v')
      
      w <- W.window 

      let widthProp  = newJSRef (gett "width" cssRef) (sett "width" cssRef)
          heightProp = newJSRef (gett "height" cssRef) (sett "height" cssRef)
          idProp     = newJSRef (getAttr "id" htmlRep) (\v -> setAttr_ "id" v htmlRep)
          onkeydownProp = onkeydown w

      writeJSRef idProp freshId

      return WindowClass {
           _windowGetId = readIORef identRef
         , _windowSetId = undefined
         , _windowGetStyle = readIORef styleRef
         
         , _windowGetRect = do
            p <- self # windowGetPosition
            s <- self # windowGetSize
            return (rect p s)

         , _windowGetPosition = readIORef posRef

         , _windowSetSize = \rect -> do
            let size = rectSize rect

            writeJSRef widthProp (sizeW size)
            writeJSRef heightProp (sizeH size)

            return ()

         , _windowGetSize = do
            w <- readJSRef widthProp
            h <- readJSRef heightProp
            return $ sz w h
            
         , _windowGetLabel = return "Window"

         -- child management
         , _windowAddChild = \c -> modifyIORef childrenRef (++ [c])

         , _windowDestroyChildren = readIORef childrenRef >>= mapM_ windowDestroy

         , _windowFindWindow = findWindow' self
         , _windowFindWindowByName = error "_windowFindWindowByName not implemented"
         , _windowGetChildren = readIORef childrenRef
         , _windowRemoveChild = \w -> do
            ident <- w # windowGetId
            c <- readIORef childrenRef
            r <- partitionByWindowId ident c 
            case r of
               Nothing      -> mapM_ ((flip windowRemoveChild) w) c 
               Just (l,a,r) -> a # windowDestroy >> writeIORef childrenRef (l ++ r)

         -- sibling and parent management functions
         , _windowGetGrandParent = do
            p <- self # windowGetParent
            maybe (return Nothing) windowGetParent p

         , _windowGetNextSibling = withSiblings self $ \r ->
            case r of
               Nothing          -> return Nothing
               Just (l,_,r:rs)  -> return (Just r)
               _                -> return Nothing

         , _windowGetPrevSibling = withSiblings self $ \r ->
            case r of
               Nothing       -> return Nothing
               Just (ls,_,_) -> return (Just $ last ls )

         , _windowGetParent = return p
         
         , _windowIsDescendant = \w -> do 
            id <- w # windowGetId
            r <- self # windowFindWindow $ id
            return $ isJust r

         , _windowReparent = \newParent -> do
            parent <- self # windowGetParent
            maybe (return ()) (\parent -> windowRemoveChild parent self) parent
            self # windowAddChild $ newParent
            return False

         -- window deletion functions
         , _windowClose = error "_windowClose not implemented"
         , _windowDestroy = do
            ident <- readIORef identRef 
            putStrLn ("destroy " ++ show ident) 
            return False

         , _windowIsBeingDeleted = return False

         -- window status functions
         , _windowHide = error "_windowHide not implemented"
         , _windowIsEnabled = error "_windowIsEnabled not implemented" 
         , _windowIsExposed = error "_windowIsExposed not implemented" 
         , _windowIsShown = error "_windowIsShown not implemented"
         , _windowIsShownOnScreen = error "_windowIsShownOnScreen not implemented"
         , _windowDisable = error "_windowDisable not implemented"
         , _windowEnable = error "_windowEnable not implemented"
         , _windowShow = do
            return True

         , _windowRefresh = \repaintBg -> do

            gc <- self # windowGetGraphicsContext
            rect <- self # windowGetRect
            graphicsContextClear gc rect

            freshId <- idCreate
            pe <- new $ paintEvent freshId

            let this :: Object
                this = upcast self 

            pe # eventSetEventObject $ this

            self # evtHandlerProcessEvent $ (upcast pe)

            return ()

         , _windowGetClientSize = return sizeZero

        , _windowGetGraphicsContext = 
            let
              getDC = do
                mbDc <- readIORef gcRef
                maybe cont return mbDc

              cont = do
                doc   <- document
                c     <- createElement doc (str "canvas")
                ident <- readIORef identRef
                
                let cid :: JSString
                    cid = str ("c-" ++ show ident)
                
                setAttr_ "id" cid c
                fc <- htmlRep # firstChild
                
                (htmlRep # insertBefore) c fc

                let mbCanvas :: Maybe HTMLCanvasElement
                    mbCanvas = cast c

                when (not $ isJust mbCanvas) (error "failed to cast to canvas element")

                let canvas = fromJust mbCanvas
                size <- self # windowGetSize
                
                let w  = sizeW size
                    h  = sizeH size
                
                setAttr_ "width" w canvas 
                setAttr_ "height" h canvas 

                native2DDC <- canvas # get2DContext 

                when (not $ isJust native2DDC) (error "failed to obtain 2d context")

                cs <- new $ canvasGraphicsContext (fromJust native2DDC)
                writeIORef gcRef (Just cs)

                self # windowGetGraphicsContext

            in getDC

         , _windowOnKeyChar = \handler -> 
            let 
              attachHandler = do
                
                let cb e = do
                      _trace (toJS "cb" :: JSString)
                      keyEventId <- idCreate
                      jsKeyCode  <- getAttr "keyCode" e
                      keyEvt     <- new $ keyEvent keyEventId 0 0 jsKeyCode ""
                      evtHandlerProcessEvent self (upcast keyEvt)
                      return ()

                writeJSRef onkeydownProp cb
                _trace (toJS "write cb" :: JSString)

              evtHandler event = do 
                let keyEvent :: Maybe KeyEvent
                    keyEvent = downcast event

                when (not $ isJust keyEvent) (_trace 123 >> error "not of type KeyEvent")

                eventKey <- eventKeyFromEvent (fromJust keyEvent)
                handler eventKey
            
            in do attachHandler
                  _trace (toJS "onkeychar" :: JSString)
                  evtHandlerBind self wxEVT_CHAR evtHandler idAny idAny

         , _windowGetOnKeyChar = do
            _trace (toJS "getonkeychar" :: JSString) 
            return $ const (return ())
         , _windowTail = tail
      }

parseInt :: String -> Int
parseInt = read . takeWhile isDigit

partitionByWindowId :: Id -> [Window] -> IO (Maybe ([Window], Window, [Window]))
partitionByWindowId id = partition (\w -> w # windowGetId >>= return . ((==) id))

partition :: (a -> IO Bool) -> [a] -> IO (Maybe ([a],a,[a]))
partition = partition' ([],[])
   where
   partition' _     f []     = return Nothing
   partition' (l,_) f (w:ws) = do
      test <- f w
      if test
         then return $ Just (l,w,ws)
         else partition' (l ++ [w],[]) f ws

windowOnPaint :: Window_ a -> (GraphicsContext -> Rect -> IO ()) -> IO ()
windowOnPaint w handler =
   (w # evtHandlerBind) wxEVT_PAINT (onPaint rectZero) idAny idAny
   where
   onPaint r e = do
      Just obj <- e # eventGetEventObject
      let window :: Maybe Window 
          window = downcast obj

      when (not $ isJust window) (error "did not receive a window")

      let w :: Window
          w = fromJust window

      gc <- w # windowGetGraphicsContext
      view <- w # windowGetViewRect

      handler gc view

--windowOnKeyChar :: Window_ a -> (EventKey -> IO ()) -> IO ()
--windowOnKeyChar window handler =
--  evtHandlerBind window wxEVT_CHAR evtHandler idAny idAny
--  where
--  evtHandler event = do 
--    let keyEvent :: Maybe KeyEvent
--        keyEvent = downcast event

--    when (not $ isJust keyEvent) (error "not of type KeyEvent")

--    eventKey <- eventKeyFromEvent (fromJust keyEvent)
--    handler eventKey

--windowOnKeyChar :: Window a -> (EventKey -> IO ()) -> IO ()
--windowOnKeyChar window handler
--  = windowOnEvent window [wxEVT_CHAR] handler eventHandler
--  where
--    eventHandler event
--      = do eventKey <- eventKeyFromEvent (objectCast event)
--           handler eventKey

windowGetOnKeyChar = error "not implemented"

{--------------------------------------------------------------------------------
  Windows
--------------------------------------------------------------------------------}
-- | Get logical view rectangle, adjusted for scrolling.
windowGetViewRect :: Window_ a -> IO Rect
windowGetViewRect window
  = do size <- windowGetClientSize window
       org  <- windowGetViewStart window
       return (rect org size)

-- | Get logical view start, adjusted for scrolling.
windowGetViewStart :: Window_ a -> IO Point
windowGetViewStart window = return pointZero

  -- = do isScrolled <- objectIsScrolledWindow window
  --     -- adjust coordinates for a scrolled window
  --     if (isScrolled)
  --      then do let scrolledWindow = objectCast window
  --              (Point sx sy) <- scrolledWindowGetViewStart scrolledWindow
  --              (Point w h)   <- scrolledWindowGetScrollPixelsPerUnit scrolledWindow
  --              return (Point (w*sx) (h*sy))
  --      else return pointZero

windowGetOnPaint = undefined