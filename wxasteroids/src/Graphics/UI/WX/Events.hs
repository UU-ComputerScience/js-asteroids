
--------------------------------------------------------------------------------
{-|	Module      :  Events
	Copyright   :  (c) Daan Leijen 2003
	               (c) Shelarcy (shelarcy@gmail.com) 2006
	License     :  wxWindows

	Maintainer  :  wxhaskell-devel@lists.sourceforge.net
	Stability   :  provisional
	Portability :  portable

Define event handling. Events are parametrised by the widget that can
correspond to a certain event and the type of the event handler.
For example, the 'resize' event has type:

> Reactive w => Event w (IO ())

This means that all widgets in the 'Reactive' class can respond to
'resize' events. (and since 'Window' is an instance of this class, this
means that basically all visible widgets are reactive).

An @Event w a@ can be transformed into an attribute of type 'Attr' @w a@
using the 'on' function.

> do f <- frame [text := "test"]
>    set f [on resize := set f [text := "resizing"]]

For convenience, the 'mouse' and 'keyboard' have a serie of /event filters/:
'click', 'drag', 'enterKey', 'charKey', etc. These filters are write-only
and do not overwrite any previous mouse or keyboard handler but all stay
active at the same time. However, all filter will be overwritten again
when 'mouse' or 'keyboard' is set again. For example, the following program
makes sense:

> set w [on click := ..., on drag := ...]

But in the following program, only the handler for 'mouse' will be called:

> set w [on click := ..., on mouse := ...]

If you want to set the 'mouse' later but retain the old event filters,
you can first read the current 'mouse' handler and call it in the 
new handler (and the same for the 'keyboard' of course). This implemenation
technique is used to implement event filters themselves and is also
very useful when setting an event handler for a 'closing' event:

> set w [on closing :~ \previous -> do{ ...; previous }]

Note that you should call 'propagateEvent' (or 'Graphics.UI.WXCore.Events.skipCurrentEvent') whenever
you do not process the event yourself in an event handler. This propagates
the event to the parent event handlers and give them a chance to
handle the event in an appropiate way. This gives another elegant way to install
a 'closing' event handler:

> set w [on closing := do{ ...; propagateEvent }]
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Events where

import Graphics.UI.WXCore hiding (Event)

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
--import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Control.Monad
import Data.Char

{--------------------------------------------------------------------
   Basic events
--------------------------------------------------------------------}
-- | An event for a widget @w@ that expects an event handler of type @a@.
data Event w a  = Event (Attr w a)

-- | Transform an event to an attribute.
on :: Event w a -> Attr w a
on (Event attr)
  = attr

-- | Change the event type.
mapEvent :: (a -> b) -> (a -> b -> a) -> Event w a -> Event w b
mapEvent get set (Event attr)
  = Event (mapAttr get set attr)

{--------------------------------------------------------------------
   Event classes
--------------------------------------------------------------------}
-- | 'Selecting' widgets fire a 'select' event when an item is selected.
class Selecting w where
  -- | A 'select' event is fired when an item is selected.
  select :: Event w (IO ())

-- | 'Commanding' widgets fire a 'command' event.
class Commanding w where
  -- | A commanding event, for example a button press.
  command :: Event w (IO ())

-- | 'Reactive' widgets are almost all visible widgets on the screen.
-- TODO: remove stub
data EventMouse

class Reactive w where
  mouse     :: Event w (EventMouse -> IO ())
  keyboard  :: Event w (EventKey -> IO ())

  closing   :: Event w (IO ())
  idle      :: Event w (IO Bool)
  resize    :: Event w (IO ())
  focus     :: Event w (Bool -> IO ())
  activate  :: Event w (Bool -> IO ())

-- | 'Paint' widgets can serve as a canvas.
-- /Note:/ it is illegal to use both a 'paint' and 'paintRaw'
-- event handler at the same widget.
class Paint w where
  -- | Paint double buffered to a device context. The context is always
  -- cleared before drawing. Takes the current view rectangle (adjusted
  -- for scrolling) as an argument.
  --paint     :: Event w (DC () -> Rect -> IO ())
  paint     :: Event w (GraphicsContext -> Rect -> IO ())
  -- | Paint directly to the on-screen device context. Takes the current
  -- view rectangle and a list of dirty rectangles as arguments.\
  --paintRaw  :: Event w (DC () -> Rect -> [Rect] -> IO ())
  paintRaw  :: Event w (GraphicsContext -> Rect -> [Rect] -> IO ())
  -- | Emit a paint event to the specified widget.
  repaint   :: w -> IO ()


{--------------------------------------------------------------------
   Mouse event filters
--------------------------------------------------------------------}
--click :: Reactive w => Event w (Point -> IO ())
--click
--  = mouseFilter "click" filter
--  where
--    filter (MouseLeftDown point mod)  = isNoShiftAltControlDown mod
--    filter other                      = False

--unclick :: Reactive w => Event w (Point -> IO ())
--unclick
--  = mouseFilter "unclick" filter
--  where
--    filter (MouseLeftUp point mod)  = isNoShiftAltControlDown mod
--    filter other                    = False

--doubleClick :: Reactive w => Event w (Point -> IO ())
--doubleClick
--  = mouseFilter "doubleClick" filter
--  where
--    filter (MouseLeftDClick point mod) = isNoShiftAltControlDown mod
--    filter other                       = False

--drag :: Reactive w => Event w (Point -> IO ())
--drag
--  = mouseFilter "drag" filter
--  where
--    filter (MouseLeftDrag point mod)  = isNoShiftAltControlDown mod
--    filter other                      = False

--motion :: Reactive w => Event w (Point -> IO ())
--motion
--  = mouseFilter "motion" filter
--  where
--    filter (MouseMotion point mod)  = isNoShiftAltControlDown mod
--    filter other                    = False

--clickRight :: Reactive w => Event w (Point -> IO ())
--clickRight
--  = mouseFilter "clickRight" filter
--  where
--    filter (MouseRightDown point mod)  = isNoShiftAltControlDown mod
--    filter other                       = False

--unclickRight :: Reactive w => Event w (Point -> IO ())
--unclickRight
--  = mouseFilter "unclickRight" filter
--  where
--    filter (MouseRightUp point mod)  = isNoShiftAltControlDown mod
--    filter other                     = False

--enter :: Reactive w => Event w (Point -> IO ())
--enter
--  = mouseFilter "enter" filter
--  where
--    filter (MouseEnter point mod)  = True
--    filter other                   = False

--leave :: Reactive w => Event w (Point -> IO ())
--leave
--  = mouseFilter "leave" filter
--  where
--    filter (MouseLeave point mod)  = True
--    filter other                   = False

--mouseFilter :: Reactive w => String -> (EventMouse -> Bool) -> Event w (Point -> IO ())
--mouseFilter name filter
--  = mapEvent get set mouse
--  where
--    get prev x
--      = ioError (userError ("WX.Events: the " ++ name ++ " event is write-only."))

--    set prev new mouseEvent
--      = if (filter mouseEvent) 
--         then new (mousePos mouseEvent)
--         else prev mouseEvent

{--------------------------------------------------------------------
  Keyboard filter events
--------------------------------------------------------------------}
rebind :: Event w (IO ()) -> Event w (IO ())
rebind event
  = mapEvent get set event
  where
    get prev
      = prev

    set new prev
      = new

--enterKey,tabKey,escKey,helpKey,delKey,homeKey,endKey :: Reactive w => Event w (IO ())
--pgupKey,pgdownKey,downKey,upKey,leftKey,rightKey :: Reactive w => Event w (IO ())
--enterKey  = key KeyReturn
--tabKey    = key KeyTab
--escKey    = key KeyEscape
--helpKey   = key KeyHelp
--delKey    = key KeyDelete
--homeKey   = key KeyHome
--endKey    = key KeyEnd
--pgupKey   = key KeyPageUp
--pgdownKey = key KeyPageDown
--downKey   = key KeyDown
--upKey     = key KeyUp
leftKey, rightKey :: Reactive w => Event w (IO ())
leftKey   = key KeyLeft
rightKey  = key KeyRight

charKey :: Reactive w => Char -> Event w (IO ())
charKey c
  -- = key (KeyChar c)
  = key (KeyChar (toUpper c))

key :: Reactive w => Key -> Event w (IO ())
key k
  = keyboardFilter "key" filter
  where
    filter (EventKey x mod pt)  = k==x


anyKey :: Reactive w => Event w (Key -> IO ())
anyKey
  = keyboardFilter1 "anyKey" (const True)

--mapEvent :: ((EventKey -> IO ()) -> b) 
--          -> ((EventKey -> IO ()) -> IO () -> (EventKey -> IO ())) 
--          -> Event w (EventKey -> IO ()) -> Event w (IO ())

--set :: 
keyboardFilter :: Reactive w => String -> (EventKey -> Bool) -> Event w (IO ())
keyboardFilter name filter
  = mapEvent get set keyboard
  where
    get prev
      = ioError (userError ("WX.Events: the " ++ name ++ " event is write-only."))

    set prev new keyboardEvent
      = do when (filter keyboardEvent) new
           prev keyboardEvent

keyboardFilter1 :: Reactive w => String -> (EventKey -> Bool) -> Event w (Key -> IO ())
keyboardFilter1 name filter
  = mapEvent get set keyboard
  where
    get prev key
      = ioError (userError ("WX.Events: the " ++ name ++ " event is write-only."))

    set prev new keyboardEvent
      = if (filter keyboardEvent) 
         then new (keyKey keyboardEvent)
         else prev keyboardEvent


{--------------------------------------------------------------------
   Calender event filters
--------------------------------------------------------------------}
--calendarEvent :: Event (CalendarCtrl a) (EventCalendar -> IO ())
--calendarEvent
--  = newEvent "calendarEvent" calendarCtrlGetOnCalEvent calendarCtrlOnCalEvent


{--------------------------------------------------------------------
   Generic event creators
-------------------------------------------------------------------}
-- | Create a new event from a get and set function.
newEvent :: String -> (w -> IO a) -> (w -> a -> IO ()) -> Event w a
newEvent name getter setter
  = Event (newAttr name getter setter)