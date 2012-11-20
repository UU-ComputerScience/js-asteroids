{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Graphics.UI.WXCore.Window (
    module Graphics.UI.WXCore.WindowClass
   ,windowCreate
   ,windowTimerCreate
   ,window
   ,windowGetOnPaint
   ,windowOnPaint
) where

import LightOO
import Data.IORef
import Data.Maybe
import Data.Typeable
import Control.Monad
import Graphics.UI.WXCore.WindowClass
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.EvtHandler
import Graphics.UI.WXCore.Timer
import Graphics.UI.WXCore.GraphicsContextClass
import Graphics.UI.WXCore.Event
import Graphics.UI.WXCore.PaintEvent
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.KeyEventClass

windowTimerCreate :: Window -> IO Timer
windowTimerCreate w = new $ timer (upcast w) idAny

windowCreate p id rect style = new $ window p id rect style  

window p id rect style tail self = do
   w <- window' p id (rectTopLeft rect) (rectSize rect) style tail self
   maybe (return ()) (\p -> p # windowAddChild $ w) p
   return w   

--window :: Maybe (Window a) -> Id -> Rect -> Style -> IO (t1 -> a) -> t1 -> IO (Window a)
window' p id pos size style = 
   (wrapper `extends` evthandler) noOverride set_EvtHandler_Tail

   where
   -- helper methods
   --findWindow' self id = do
   --   id' <- getId self
   --   putStrLn ("find: " ++ show id')
   --   if id == id'
   --      then return $ Just (unsafeCoerce self)
   --      else recurseInChildren
   --   where
   --   recurseInChildren = do
   --      c <- readVar _children
   --      r <- filterM (\w -> getId w >>= \i -> return (i == id)) c
   --      case r of
   --         []    -> foldM (\a w -> findWindow w id >>= \r -> return $ a `mplus` r)
   --                        Nothing c 
   --         (x:_) -> return $ Just x

   findWindow' self id = do
      id' <- self # windowGetId
      if id == id'
         then return $ Just self
         else recurseInChildren
      where
      recurseInChildren = do
         c <- self # windowGetChildren
         r <- filterM (\w -> w # windowGetId >>= \i -> return $ i == id) c
         case r of
            []    -> foldM (\a w -> (w # windowFindWindow) id >>= \r -> return $ a `mplus` r)
                           Nothing c 
            (x:_) -> return $ Just x

   withSiblings self f = do
      p <- self # windowGetParent
      case p of
         Nothing -> return Nothing
         Just p  -> do { c <- p # windowGetChildren
                       ; id <- self # windowGetId
                       ; r <- partitionByWindowId id c
                       ; f r
                       }
                  
   -- bound to an onpaint event, functions that modify window state might
   -- cause a repaint event to be put onto the event queue which indirectly
   -- causes this function to execute     
   repaint self = do
      freshId <- idCreate
      pe <- new $ paintEvent id

      let this :: Object
          this = upcast self 

      pe # eventSetEventObject $ this
      
      self # evtHandlerProcessEvent $ (upcast pe)
      return ()

   wrapper tail super self = do
      children' <- newIORef [] :: IO (IORef [Window])
      size' <- newIORef size :: IO (IORef Size)
      style' <- newIORef style
      id' <- newIORef id
      pos' <- newIORef pos :: IO (IORef Point)
      dc <- newIORef Nothing

      let div = undefined
      let d = undefined

      return WindowClass {
           _windowGetId = readIORef id'
         , _windowSetId = undefined
         , _windowGetStyle = readIORef style'
         
         , _windowGetRect = do
            p <- readIORef pos'
            s <- readIORef size'
            return (rect p s)

         , _windowGetPosition = readIORef pos'
         , _windowGetSize = readIORef size'

         , _windowSetSize = undefined
            
         , _windowGetLabel = return "Window"

         -- child management
         , _windowAddChild = \c -> modifyIORef children' (++ [c])

         , _windowDestroyChildren = readIORef children' >>= mapM_ windowDestroy

         , _windowFindWindow = findWindow' self
         , _windowFindWindowByName = error "_windowFindWindowByName not implemented"
         , _windowGetChildren = readIORef children'
         , _windowRemoveChild = \w -> do
            id <- w # windowGetId
            c <- readIORef children'
            r <- partitionByWindowId id c 
            case r of
               Nothing      -> mapM_ ((flip windowRemoveChild) w) c 
               Just (l,a,r) -> a # windowDestroy >> writeIORef children' (l ++ r)

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
         , _windowDestroy = putStrLn ("destroy " ++ show id) >> return False
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
            self # repaint
            return True

         , _windowRefresh = undefined
         , _windowGetClientSize = return sizeZero
         , _windowGetGraphicsContext =
            let
              getDC = do
                mbDc <- readIORef dc
                maybe cont fromJust mbDc

              cont = do
                c <- createElement d (toJS "canvas")
                set "id" (toJS $ "c-" ++ show id) c
                fc <- div # firstChild
                div # insertBefore $ c fc  
                return undefined

            in getDC
         , _windowOnKeyChar = \handler -> 
            let 
              evtHandler event = do 
                let keyEvent :: Maybe KeyEvent
                    keyEvent = downcast event

                when (not $ isJust keyEvent) (error "not of type KeyEvent")

                eventKey <- eventKeyFromEvent (fromJust keyEvent)
                handler eventKey 
            
            in evtHandlerBind self wxEVT_CHAR evtHandler idAny idAny

         , _windowGetOnKeyChar = undefined
         
         , _windowTail = tail
      }

set = undefined
toJS = undefined
insertBefore = undefined
firstChild = undefined
createElement = undefined

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

      let Just w = window

      gc <- w # windowGetGraphicsContext
      view <- w # windowGetViewRect

      handler gc view

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