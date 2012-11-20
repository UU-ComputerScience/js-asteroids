{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
--------------------------------------------------------------------------------
{-|	Module      :  Window
	Copyright   :  (c) Daan Leijen 2003
	License     :  wxWindows

	Maintainer  :  wxhaskell-devel@lists.sourceforge.net
	Stability   :  provisional
	Portability :  portable

Exports default instances for generic windows.

* Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.             
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Window where

import Graphics.UI.WXCore

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
--import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events
import LightOO

{--------------------------------------------------------------------------------
  ScrolledWindow
--------------------------------------------------------------------------------}
-- | A scrollable window. Use 'virtualSize' and 'scrollRate' to set the scrollbar
-- behaviour.
--
-- * Attributes: 'scrollRate'
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
--scrolledWindow :: Window a -> [Prop (ScrolledWindow ())] -> IO (ScrolledWindow ())
--scrolledWindow parent props
--  = feed2 props 0 $
--    initialContainer $ \id rect -> \props style ->
--    do sw <- scrolledWindowCreate parent id rect style
--       set sw props
--       return sw


-- | The horizontal and vertical scroll rate of scrolled window. Use @0@ to disable 
-- scrolling in that direction.
--scrollRate :: Attr (ScrolledWindow a) Size
--scrollRate
--  = newAttr "scrollRate" getter setter
--  where
--    getter sw
--      = do p <- scrolledWindowGetScrollPixelsPerUnit sw
--           return (sizeFromPoint p)
     
--    setter sw size
--      = scrolledWindowSetScrollRate sw (sizeW size) (sizeH size)


{--------------------------------------------------------------------------------
  Plain window
--------------------------------------------------------------------------------}
-- | Create a plain window. Can be used to define custom controls for example.
--
-- * Attributes: 'rootParent', 'frameParent', 'tabTraversal'
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--window :: Window a -> [Prop Window] -> IO Window
window :: Maybe Window -> [Prop Window] -> IO Window
window parent props
  = feed2 props 0 $
    initialWindow $ \id rect -> \props flags ->
    do w <- windowCreate parent id rect flags
       set w props
       return w


{--------------------------------------------------------------------------------
  Properties
--------------------------------------------------------------------------------}
-- | Helper function that retrieves initial window settings, including
-- |identity|, |style|, and |area| (or |position| and |outerSize|).
--initialWindow :: (Id -> Rect -> [Prop Window] -> Style -> a) -> [Prop Window] -> Style -> a
initialWindow :: (Id -> Rect -> [Prop (Window_ w)] -> Style -> a) -> [Prop (Window_ w)] -> Style -> a
initialWindow cont 
  = initialIdentity $ \id ->
    initialArea     $ \rect ->
    initialStyle    $ 
    --initialBorder   $
    cont id rect 

-- | Helper function that retrieves initial window settings, including |clipChildren|
-- and |fullRepaintOnResize|.
--initialContainer :: (Id -> Rect -> [Prop Window] -> Style -> a) -> [Prop Window] -> Style -> a
initialContainer :: (Id -> Rect -> [Prop (Window_ w)] -> Style -> a) -> [Prop (Window_ w)] -> Style -> a
initialContainer cont
  = initialWindow $ \id rect ->
    initialFullRepaintOnResize $ 
    initialClipChildren        $ 
    cont id rect 


instance Able (Window_ a) where
  enabled
    = newAttr "enabled" windowIsEnabled setter
    where
      setter w enable
        | enable    = unitIO $ windowEnable w
        | otherwise = unitIO $ windowDisable w


instance Textual (Window_ a) where
  text
    = reflectiveAttr "text" getter setter
    where
      getter w
        = fst (getset w)
      setter w x
        = snd (getset w) x

      --getset w
      --  = ifInstanceOf w classComboBox
      --      (\cb -> (comboBoxGetValue cb, \s -> do comboBoxClear cb; comboBoxAppend cb s)) $
      --    ifInstanceOf w classTextCtrl
      --      (\tc -> (textCtrlGetValue tc, \s -> do textCtrlChangeValue tc s)) $
      --      (windowGetLabel w,windowSetLabel w)
      getset w
        = (windowGetLabel w,windowSetLabel w)

  --appendText w s
  --  = ifInstanceOf w classComboBox
  --      (\cb -> comboBoxAppend cb s) $
  --    ifInstanceOf w classTextCtrl
  --      (\tc -> textCtrlAppendText tc s)
  --      (set w [text :~ (++s)])
  appendText w s
    = (set w [text :~ (++s)])


-- | Retrieve the initial title from the |text| attribute.
initialText :: Textual w => (String -> [Prop w] -> a) -> [Prop w] -> a
initialText cont props 
  = withProperty text "" cont props


--instance Dimensions Window where
--  outerSize
--    = newAttr "size" undefined setSize
--    where
--      setSize w sz
--        = windowSetSize w (rect (pt (-1) (-1)) sz) wxSIZE_USE_EXISTING

--  area
--    = newAttr "area" windowGetRect setArea
--    where
--      setArea w rect
--        = windowSetSize w rect wxSIZE_USE_EXISTING

--  bestSize
--    = readAttr "bestSize" windowGetEffectiveMinSize

--  position
--    = newAttr "position" windowGetPosition windowMove

--  clientSize
--    = newAttr "clientSize" windowGetClientSize windowSetClientSize

--  virtualSize
--    = newAttr "virtualSize" windowGetVirtualSize windowSetVirtualSize

instance Dimensions (Window_ a) where
  outerSize
    = newAttr "size" windowGetSize setSize
    where
      setSize w sz
        = windowSetSize w (rect (pt (-1) (-1)) sz) -- wxSIZE_USE_EXISTING

  area
    = newAttr "area" windowGetRect setArea
    where
      setArea w rect
        = windowSetSize w rect -- wxSIZE_USE_EXISTING

  bestSize
    = readAttr "bestSize" windowGetEffectiveMinSize

  position
    = newAttr "position" windowGetPosition windowMove

  clientSize
    = newAttr "clientSize" windowGetClientSize windowSetClientSize

  virtualSize
    = newAttr "virtualSize" windowGetVirtualSize windowSetVirtualSize


instance Sized (Window_ a) where
  size  = outerSize

-- | Retrieve the initial creation area from the |area|, or the |position| and
-- |outerSize| properties.
--initialArea :: Dimensions w => (Rect -> [Prop w] -> a) -> [Prop w] -> a
--initialArea cont props
--  = case findProperty area rectNull props of
--      Just (rect,props') -> cont rect props'
--      Nothing 
--        -> case findProperty position pointNull props of
--             Just (p,props') -> case findProperty outerSize sizeNull props of
--                                  Just (sz,props'') -> cont (rect p sz) props''
--                                  Nothing           -> cont (rect p sizeNull) props'
--             Nothing         -> case findProperty outerSize sizeNull props of
--                                  Just (sz,props')  -> cont (rect pointNull sz) props'
--                                  Nothing           -> cont rectNull props
-- default to sizeZero if no dimensions are set
initialArea :: Dimensions w => (Rect -> [Prop w] -> a) -> [Prop w] -> a
initialArea cont props
  = case findProperty area rectZero props of
      Just (rect,props') -> cont rect props'
      Nothing 
        -> case findProperty position pointZero props of
             Just (p,props') -> case findProperty outerSize sizeZero props of
                                  Just (sz,props'') -> cont (rect p sz) props''
                                  Nothing           -> cont (rect p sizeZero) props'
             Nothing         -> case findProperty outerSize sizeZero props of
                                  Just (sz,props')  -> cont (rect pointZero sz) props'
                                  Nothing           -> cont rectZero props

instance Colored (Window_ a) where
  bgcolor
    = newAttr "bgcolor" windowGetBackgroundColour (\w x -> do{ windowSetBackgroundColour w x; return ()})

  color
    = newAttr "color" windowGetForegroundColour (\w x -> do windowSetForegroundColour w x; return ())


--instance Literate (Window_ a) where
--  font
--    = newAttr "font" getter setter
--    where
--      getter w
--        = ifInstanceOf w classTextCtrl
--           (\textCtrl -> bracket (textCtrlGetDefaultStyle textCtrl)
--                                 (textAttrDelete)
--                                 (\attr -> do hasFont <- textAttrHasFont attr
--                                              if (hasFont) 
--                                                then getFont (textAttrGetFont attr) 
--                                                else getFont (windowGetFont w)))
--           (getFont (windowGetFont w))
--        where
--          getFont io
--            = bracket io fontDelete fontGetFontStyle 

--      setter w info
--        = ifInstanceOf w classTextCtrl
--           (\textCtrl -> bracket (textAttrCreateDefault)
--                                 (textAttrDelete)
--                                 (\attr -> withFontStyle info $ \fnt ->
--                                           do textAttrSetFont attr fnt
--                                              textCtrlSetDefaultStyle textCtrl attr
--                                              return ()))
--           (withFontStyle info $ \fnt ->
--            do windowSetFont w fnt
--               return ())

--  textColor
--    = newAttr "textcolor" getter setter
--    where
--      getter w
--        = ifInstanceOf w classTextCtrl
--           (\textCtrl -> bracket (textCtrlGetDefaultStyle textCtrl)
--                                 (textAttrDelete)
--                                 (\attr -> do hasColor <- textAttrHasTextColour attr
--                                              if (hasColor) then textAttrGetTextColour attr
--                                                            else get w color))
--           (get w color)

--      setter w c
--        = ifInstanceOf w classTextCtrl
--           (\textCtrl -> bracket (textAttrCreateDefault)
--                                 (textAttrDelete)
--                                 (\attr -> do textAttrSetTextColour attr c
--                                              textCtrlSetDefaultStyle textCtrl attr
--                                              return ()))
--           (set w [color := c])

--  textBgcolor
--    = newAttr "textbgcolor" getter setter
--    where
--      getter w
--        = ifInstanceOf w classTextCtrl
--           (\textCtrl -> bracket (textCtrlGetDefaultStyle textCtrl)
--                                 (textAttrDelete)
--                                 (\attr -> do hasColor <- textAttrHasBackgroundColour attr
--                                              if (hasColor) then textAttrGetBackgroundColour attr
--                                                            else get w bgcolor))
--           (get w bgcolor)

--      setter w c
--       = ifInstanceOf w classTextCtrl
--           (\textCtrl -> bracket (textAttrCreateDefault)
--                                 (textAttrDelete)
--                                 (\attr -> do textAttrSetBackgroundColour attr c
--                                              textCtrlSetDefaultStyle textCtrl attr
--                                              return ()))
--           (set w [bgcolor := c])

  

instance Visible (Window_ a) where
  visible
    = newAttr "visible" windowIsShown setVisible
    where
      setVisible w vis
        = if vis
           then do{ windowShow w; windowRaise w }
           else unitIO (windowHide w)

  refresh w
    = windowRefresh w True

  fullRepaintOnResize
    = reflectiveAttr "fullRepaintOnResize" getFlag setFlag
    where
      getFlag w
        = do s <- get w style
             return (not (bitsSet wxNO_FULL_REPAINT_ON_RESIZE s))

      setFlag w repaint
        = set w [style :~ \stl -> if repaint 
                                   then stl .-. wxNO_FULL_REPAINT_ON_RESIZE 
                                   else stl .+. wxNO_FULL_REPAINT_ON_RESIZE]

instance Parent (Window_ a) where
  children
    = readAttr "children" windowChildren

  clipChildren
    = reflectiveAttr "clipChildren" getFlag setFlag
    where
      getFlag w
        = do s <- get w style
             return (bitsSet wxCLIP_CHILDREN s)
      setFlag w clip
        = set w [style :~ \stl -> if clip
                                   then stl .+. wxCLIP_CHILDREN
                                   else stl .-. wxCLIP_CHILDREN]


-- | Helper function that transforms the style accordding
-- to the 'fullRepaintOnResize' flag in of the properties
initialFullRepaintOnResize :: Visible w => ([Prop w] -> Style -> a) -> [Prop w] -> Style -> a
initialFullRepaintOnResize 
  = withStylePropertyNot fullRepaintOnResize wxNO_FULL_REPAINT_ON_RESIZE


-- | Helper function that transforms the style accordding
-- to the 'clipChildren' flag out of the properties
initialClipChildren :: Parent w => ([Prop w] -> Style -> a) -> [Prop w] -> Style -> a
initialClipChildren 
  = withStyleProperty clipChildren wxCLIP_CHILDREN




instance Child (Window_ a) where
  parent
    = readAttr "parent" windowGetParent

-- | Ensure that a widget is refitted inside a window when
-- its size changes, for example when the 'text' of a 
-- 'staticText' control changes. (calls 'windowReFit')
refit :: Window_ a -> IO ()
refit w
  = windowReFit w

-- | Ensure that a widget is refitted inside a window when
-- its size changes, for example when the 'text' of a 
-- 'staticText' control changes. Always resizes the
-- window to its minimal acceptable size. (calls 'windowReFitMinimal')
refitMinimal :: Window_ a -> IO ()
refitMinimal w
  = windowReFitMinimal w

-- | The ultimate root parent of the widget.
rootParent :: ReadAttr (Window_ a) Window
rootParent 
  = readAttr "rootParent" windowGetRootParent 

-- | The parent frame or dialog of a widget.
frameParent :: ReadAttr (Window_ a) Window
frameParent 
  = readAttr "frameParent" windowGetFrameParent 


-- | Enable (or disable) tab-traversal. (= wxTAB_TRAVERSAL)
--tabTraversal :: Attr (Window_ a) Bool
--tabTraversal
--  = newAttr "tabTraversal" getter setter
--  where
--    getter w
--      = do st <- get w style
--           return (bitsSet wxTAB_TRAVERSAL st)
--    setter w enable
--      = set w [style :~ \stl -> if enable then stl .+. wxTAB_TRAVERSAL else stl .-. wxTAB_TRAVERSAL]


instance Identity (Window_ a) where
  identity
    = reflectiveAttr "identity" windowGetId windowSetId

-- | Helper function that retrieves the initial |identity|.
initialIdentity :: Identity w => (Id -> [Prop w] -> a) -> [Prop w] -> a
initialIdentity
  = withProperty identity idAny

instance Styled (Window_ a) where
  style
    = reflectiveAttr "style" windowGetWindowStyleFlag windowSetWindowStyleFlag

-- | Helper function that retrieves the initial |style|.
initialStyle :: Styled w => ([Prop w] -> Style -> a) -> [Prop w] -> Style -> a
initialStyle cont props stl
  = withProperty style stl (\stl' props' -> cont props' stl') props

--instance Tipped (Window a) where
--  tooltip
--    = newAttr "tooltip" windowGetToolTip windowSetToolTip

{-
instance Help (Window a) where
  help
    = newAttr "help" windowSetHelpText windowGetHelpText
-}

{--------------------------------------------------------------------------------
  Borders
--------------------------------------------------------------------------------}
--instance Bordered (Window a) where
--  border 
--    = reflectiveAttr "border" getter setter
--    where
--      getter w
--        = do st <- get w style
--             return (fromBitMask st)
--      setter w b
--        = set w [style :~ \stl -> setBitMask b stl]

--initialBorder cont props style
--  = case filterProperty border props of
--      (PropValue x, ps)  -> cont ps (setBitMask x style) 
--      (PropModify f, ps) -> cont ps (setBitMask (f (fromBitMask style)) style)
--      (PropNone, ps)     -> cont ps style

{--------------------------------------------------------------------------------
  Events
--------------------------------------------------------------------------------}
instance Reactive (Window_ a) where
  mouse     = newEvent "mouse" windowGetOnMouse (\w h -> windowOnMouse w True h)
  keyboard  = newEvent "keyboard" windowGetOnKeyChar windowOnKeyChar
  closing   = newEvent "closing" windowGetOnClose windowOnClose
  idle      = newEvent "idle" windowGetOnIdle windowOnIdle
  resize    = newEvent "resize" windowGetOnSize windowOnSize
  focus     = newEvent "focus" windowGetOnFocus windowOnFocus
  activate  = newEvent "activate" windowGetOnActivate windowOnActivate

instance Paint (Window_ a) where
  paint     = newEvent "paint" windowGetOnPaint (\w h -> windowOnPaint w h)
  paintRaw  = error "paintRaw  not implemented" -- newEvent "paintRaw" windowGetOnPaintRaw (\w h -> windowOnPaintRaw w h)
  repaint w = windowRefresh w False