{-# LANGUAGE CPP, DatatypeContexts, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS -pgmP cpp #-}
module Graphics.UI.WXCore.Types where

import Data.Bits
import Data.Typeable
import Data.Ix
-- UHC js has no data.word
--import Data.Word
import Data.IORef
#ifndef __UHC__
import System.IO.Unsafe
#endif

type Id = Int
type EventType = Int
type Style = Int

data Image_ a
type Image = Image_ ()

data Bitmap_ a
type Bitmap = Bitmap_ ()

infixl 5 .+.
infixl 5 .-.
--infix 5 #

-- | Reverse application, i.e. @x # f@ = @f x@.
-- Useful for an object oriented style of programming.
--
-- > (frame # frameSetTitle) "hi"
--
--( # ) :: obj -> (obj -> a) -> a
--object # method   = method object


{--------------------------------------------------------------------------------
  Bitmasks
--------------------------------------------------------------------------------}
type BitFlag = Int

-- | Bitwise /or/ of two bit masks.
(.+.) :: Int -> Int -> Int
(.+.) i j
  = i .|. j

-- | Unset certain bits in a bitmask.
(.-.) :: Int -> BitFlag -> Int
(.-.) i j
  = i .&. complement j

-- | Bitwise /or/ of a list of bit masks.
bits :: [Int] -> Int
bits xs
  = foldr (.+.) 0 xs

-- | (@bitsSet mask i@) tests if all bits in @mask@ are also set in @i@.
bitsSet :: Int -> Int -> Bool
bitsSet mask i
  = (i .&. mask == mask)


{--------------------------------------------------------------------------------
  Id
--------------------------------------------------------------------------------}
wxID_HIGHEST :: Int
wxID_HIGHEST = 5999

wxID_ANY = -1

{-# NOINLINE varTopId #-}
varTopId :: Var Id
varTopId
  = unsafePerformIO (varCreate (wxID_HIGHEST+1))

-- | When creating a new window you may specify 'idAny' to let wxWindows
-- assign an unused identifier to it automatically. Furthermore, it can be
-- used in an event connection to handle events for any identifier.
idAny :: Id
idAny
  = wxID_ANY

-- | Create a new unique identifier.
idCreate :: IO Id
idCreate
  = varUpdate varTopId (+1)



{--------------------------------------------------------------------------------
  Control
--------------------------------------------------------------------------------}
-- | Ignore the result of an 'IO' action.
unitIO :: IO a -> IO ()
unitIO io
  = do io; return ()

-- | Perform an action when a test succeeds.
--when :: Bool -> IO () -> IO ()
--when = M.when

-- | Properly release resources, even in the event of an exception.
--bracket :: IO a           -- ^ computation to run first (acquire resource)
--           -> (a -> IO b) -- ^ computation to run last (release resource)
--           -> (a -> IO c) -- ^ computation to run in-between (use resource)
--           -> IO c
--bracket = CE.bracket

---- | Specialized variant of 'bracket' where the return value is not required.
--bracket_ :: IO a     -- ^ computation to run first (acquire resource)
--           -> IO b   -- ^ computation to run last (release resource)
--           -> IO c   -- ^ computation to run in-between (use resource)
--           -> IO c
--bracket_ = CE.bracket_

---- | Run some computation afterwards, even if an exception occurs.
--finally :: IO a -- ^ computation to run first
--        -> IO b -- ^ computation to run last (release resource)
--        -> IO a
--finally = CE.finally

---- | Run some computation afterwards, even if an exception occurs. Equals 'finally' but
---- with the arguments swapped.
--finalize ::  IO b -- ^ computation to run last (release resource)
--          -> IO a -- ^ computation to run first
--          -> IO a
--finalize last first
--  = finally first last

{--------------------------------------------------------------------------------
  Variables
--------------------------------------------------------------------------------}

-- | A mutable variable. Use this instead of 'MVar's or 'IORef's to accomodate for
-- future expansions with possible concurrency.
type Var a  = IORef a
--type Var a  = TVar a

-- | Create a fresh mutable variable.
varCreate :: a -> IO (Var a)
varCreate x    = newIORef x

-- | Get the value of a mutable variable.
varGet :: Var a -> IO a
varGet v    = readIORef v

-- | Set the value of a mutable variable.
varSet :: Var a -> a -> IO ()
varSet v x = writeIORef v x

-- | Swap the value of a mutable variable.
varSwap :: Var a -> a -> IO a
varSwap v x = do
                prev <- readIORef v
                writeIORef v x
                return prev

-- | Update the value of a mutable variable and return the old value.
varUpdate :: Var a -> (a -> a) -> IO a
varUpdate v f = do
                   x <- readIORef v
                   writeIORef v (f x)
                   return x


{-----------------------------------------------------------------------------------------
  Point
-----------------------------------------------------------------------------------------}
--- | Define Point type synonym for backward compatibility.
type Point = Point2 Int

-- | A point has an x and y coordinate. Coordinates are normally relative to the
-- upper-left corner of their view frame, where a positive x goes to the right and
-- a positive y to the bottom of the view.
data (Num a) => Point2 a = Point
        { pointX :: !a -- ^ x component of a point.
        , pointY :: !a -- ^ y component of a point.
        }
        deriving (Eq,Show,Read,Typeable)

-- | Construct a point.
point :: (Num a) => a -> a -> Point2 a
point x y  = Point x y

-- | Shorter function to construct a point.
pt :: (Num a) => a -> a -> Point2 a
pt x y  = Point x y

pointFromVec :: (Num a) => Vector -> Point2 a
pointFromVec (Vector x y)
  = Point (fromIntegral x) (fromIntegral y)

pointFromSize :: (Num a) => Size -> Point2 a
pointFromSize (Size w h)
  = Point (fromIntegral w) (fromIntegral h)

-- | Point at the origin.
pointZero :: (Num a) => Point2 a
pointZero
  = Point 0 0

pointMove :: (Num a) => Vector2 a -> Point2 a -> Point2 a
pointMove (Vector dx dy) (Point x y)
  = Point (x+dx) (y+dy)

pointMoveBySize :: (Num a) => Point2 a -> Size2D a -> Point2 a
pointMoveBySize (Point x y) (Size w h)  = Point (x + w) (y + h)

pointAdd :: (Num a) => Point2 a -> Point2 a -> Point2 a
pointAdd (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

pointSub :: (Num a) => Point2 a -> Point2 a -> Point2 a
pointSub (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

pointScale :: (Num a) => Point2 a -> a -> Point2 a
pointScale (Point x y) v = Point (v*x) (v*y)


instance (Num a, Ord a) => Ord (Point2 a) where
  compare (Point x1 y1) (Point x2 y2)             
    = case compare y1 y2 of
        EQ  -> compare x1 x2
        neq -> neq


instance Ix (Point2 Int) where
  range (Point x1 y1,Point x2 y2)             
    = [Point x y | y <- [y1..y2], x <- [x1..x2]]

  inRange (Point x1 y1, Point x2 y2) (Point x y)
    = (x >= x1 && x <= x2 && y >= y1 && y <= y2)

  rangeSize (Point x1 y1, Point x2 y2) 
    = let w = abs (x2 - x1) + 1
          h = abs (y2 - y1) + 1
      in w*h

  index bnd@(Point x1 y1, Point x2 y2) p@(Point x y)
    = if inRange bnd p
       then let w = abs (x2 - x1) + 1
            in (y-y1)*w + x
       else error ("Point index out of bounds: " ++ show p ++ " not in " ++ show bnd)

{-----------------------------------------------------------------------------------------
  Size
-----------------------------------------------------------------------------------------}
--- | Define Point type synonym for backward compatibility.
type Size = Size2D Int

-- | A @Size@ has a width and height.
data (Num a) => Size2D a = Size
        { sizeW :: !a -- ^ the width  of a size
        , sizeH :: !a -- ^ the height of a size
        }
        deriving (Eq,Show,Typeable)

-- | Construct a size from a width and height.
size :: (Num a) => a -> a -> Size2D a
size w h
  = Size w h

-- | Short function to construct a size
sz :: (Num a) => a -> a -> Size2D a
sz w h
  = Size w h

sizeFromPoint :: (Num a) => Point2 a -> Size2D a
sizeFromPoint (Point x y)
  = Size x y

sizeFromVec   :: (Num a) => Vector2 a -> Size2D a
sizeFromVec (Vector x y)
  = Size x y

sizeZero :: (Num a) => Size2D a
sizeZero
  = Size 0 0

-- | Return the width. (see also 'sizeW').
sizeWidth :: (Num a) => Size2D a -> a
sizeWidth (Size w h)
  = w

-- | Return the height. (see also 'sizeH').
sizeHeight :: (Num a) => Size2D a -> a
sizeHeight (Size w h)
  = h

-- | Returns 'True' if the first size totally encloses the second argument.
sizeEncloses :: (Num a, Ord a) => Size2D a -> Size2D a -> Bool
sizeEncloses (Size w0 h0) (Size w1 h1)
  = (w0 >= w1) && (h0 >= h1)

-- | The minimum of two sizes.
sizeMin :: (Num a, Ord a) => Size2D a -> Size2D a -> Size2D a
sizeMin (Size w0 h0) (Size w1 h1)
  = Size (min w0 w1) (min h0 h1)

-- | The maximum of two sizes.
sizeMax :: (Num a, Ord a) => Size2D a -> Size2D a -> Size2D a
sizeMax (Size w0 h0) (Size w1 h1)
  = Size (max w0 w1) (max h0 h1)

{-----------------------------------------------------------------------------------------
  Vector
-----------------------------------------------------------------------------------------}
--- | Define Point type synonym for backward compatibility.
type Vector = Vector2 Int

-- | A vector with an x and y delta.
data (Num a) => Vector2 a = Vector
        { vecX :: !a -- ^ delta-x component of a vector
        , vecY :: !a -- ^ delta-y component of a vector
        }
        deriving (Eq,Show,Read,Typeable)

-- | Construct a vector.
vector :: (Num a) => a -> a -> Vector2 a
vector dx dy  = Vector dx dy

-- | Short function to construct a vector.
vec :: (Num a) => a -> a -> Vector2 a
vec dx dy  = Vector dx dy

-- | A zero vector
vecZero :: (Num a) => Vector2 a
vecZero
  = Vector 0 0

vecFromPoint :: (Num a) => Point2 a -> Vector2 a
vecFromPoint (Point x y)
  = Vector x y

vecFromSize :: Size -> Vector
vecFromSize (Size w h)
  = Vector w h

vecNegate :: (Num a) => Vector2 a -> Vector2 a
vecNegate (Vector x y)
  = Vector (-x) (-y)

vecOrtogonal :: (Num a) => Vector2 a -> Vector2 a
vecOrtogonal (Vector x y) = (Vector y (-x))

vecAdd :: (Num a) => Vector2 a -> Vector2 a -> Vector2 a
vecAdd (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

vecSub :: (Num a) => Vector2 a -> Vector2 a -> Vector2 a
vecSub (Vector x1 y1) (Vector x2 y2) = Vector (x1-x2) (y1-y2)

vecScale :: (Num a) => Vector2 a -> a -> Vector2 a
vecScale (Vector x y) v = Vector (v*x) (v*y)

vecBetween :: (Num a) => Point2 a -> Point2 a -> Vector2 a
vecBetween (Point x1 y1) (Point x2 y2) = Vector (x2-x1) (y2-y1)

vecLength :: Vector -> Double
vecLength (Vector x y)
  = sqrt (fromIntegral (x*x + y*y))

vecLengthDouble :: Vector2 Double -> Double
vecLengthDouble (Vector x y)
  = sqrt (x*x + y*y)

{-----------------------------------------------------------------------------------------
  Rectangle
-----------------------------------------------------------------------------------------}
--- | Define Point type synonym for backward compatibility.
type Rect = Rect2D Int

-- | A rectangle is defined by the left x coordinate, the top y coordinate,
-- the width and the height.
data (Num a) => Rect2D a = Rect
        { rectLeft   :: !a
        , rectTop    :: !a
        , rectWidth  :: !a
        , rectHeight :: !a
        }
        deriving (Eq,Show,Read,Typeable)


rectTopLeft, rectTopRight, rectBottomLeft, rectBottomRight :: (Num a) => Rect2D a -> Point2 a
rectTopLeft     (Rect l t w h)  = Point l t
rectTopRight    (Rect l t w h)  = Point (l+w) t
rectBottomLeft  (Rect l t w h)  = Point l (t+h)
rectBottomRight (Rect l t w h)  = Point (l+w) (t+h)

rectBottom, rectRight :: (Num a) => Rect2D a -> a
rectBottom (Rect x y w h)  = y + h
rectRight  (Rect x y w h)  = x + w

-- | Create a rectangle at a certain (upper-left) point with a certain size.
rect :: (Num a) => Point2 a -> Size2D a -> Rect2D a
rect (Point x y) (Size w h)
  = Rect x y w h

-- | Construct a (positive) rectangle between two (arbitrary) points.
rectBetween :: (Num a, Ord a) => Point2 a -> Point2 a -> Rect2D a
rectBetween (Point x0 y0) (Point x1 y1)
  = Rect (min x0 x1) (min y0 y1) (abs (x1-x0)) (abs (y1-y0))

-- | An empty rectangle at (0,0).
rectZero :: (Num a) => Rect2D a
rectZero
  = Rect 0 0 0 0

-- | Get the size of a rectangle.
rectSize :: (Num a) => Rect2D a -> Size2D a
rectSize (Rect l t w h)
  = Size w h

-- | Create a rectangle of a certain size with the upper-left corner at ('pt' 0 0).
rectFromSize :: (Num a) => Size2D a -> Rect2D a
rectFromSize (Size w h)
  = Rect 0 0 w h

rectIsEmpty :: (Num a, Eq a) => Rect2D a -> Bool
rectIsEmpty (Rect l t w h)
  = (w==0 && h==0)

rectContains :: (Num a, Ord a) => Rect2D a -> Point2 a -> Bool
rectContains (Rect l t w h) (Point x y) 
  = (x >= l && x <= (l+w) && y >= t && y <= (t+h))

rectMoveTo :: (Num a) => Rect2D a -> Point2 a -> Rect2D a
rectMoveTo r p
  = rect p (rectSize r)

rectFromPoint :: (Num a) => Point2 a -> Rect2D a
rectFromPoint (Point x y)
  = Rect x y x y

rectCentralPoint :: Rect2D Int -> Point2 Int
rectCentralPoint (Rect l t w h)
  = Point (l + div w 2) (t + div h 2)

rectCentralRect :: Rect2D Int -> Size -> Rect2D Int
rectCentralRect r@(Rect l t rw rh) (Size w h)
  = let c = rectCentralPoint r
    in Rect (pointX c - (w - div w 2)) (pointY c - (h - div h 2)) w h

rectCentralPointDouble :: (Fractional a) => Rect2D a -> Point2 a
rectCentralPointDouble (Rect l t w h)
  = Point (l + w/2) (t + h/2)

rectCentralRectDouble :: (Fractional a) => Rect2D a -> Size2D a -> Rect2D a
rectCentralRectDouble r@(Rect l t rw rh) (Size w h)
  = let c = rectCentralPointDouble r
    in Rect (pointX c - (w - w/2)) (pointY c - (h - h/2)) w h


rectStretchTo :: (Num a) => Rect2D a -> Size2D a -> Rect2D a
rectStretchTo (Rect l t _ _) (Size w h)
  = Rect l t w h

rectMove :: (Num a) => Rect2D a -> Vector2 a -> Rect2D a
rectMove  (Rect x y w h) (Vector dx dy)
  = Rect (x+dx) (y+dy) w h

rectOverlaps :: (Num a, Ord a) => Rect2D a -> Rect2D a -> Bool
rectOverlaps (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2)
  = (x1+w1 >= x2 && x1 <= x2+w2) && (y1+h1 >= y2 && y1 <= y2+h2)


-- | A list with rectangles that constitute the difference between two rectangles.
rectsDiff :: (Num a, Ord a) => Rect2D a -> Rect2D a -> [Rect2D a]
rectsDiff rect1 rect2
  = subtractFittingRect rect1 (rectOverlap rect1 rect2)
  where
    -- subtractFittingRect r1 r2 subtracts r2 from r1 assuming that r2 fits inside r1
    subtractFittingRect :: (Num a, Ord a) => Rect2D a -> Rect2D a -> [Rect2D a]
    subtractFittingRect r1 r2 =
            filter (not . rectIsEmpty)
                    [ rectBetween (rectTopLeft r1) (rectTopRight r2)
                    , rectBetween (pt (rectLeft r1) (rectTop r2)) (rectBottomLeft r2)
                    , rectBetween (pt (rectLeft r1) (rectBottom r2)) (pt (rectRight r2) (rectBottom r1))
                    , rectBetween (rectTopRight r2) (rectBottomRight r1)
                    ]

rectUnion :: (Num a, Ord a) => Rect2D a -> Rect2D a -> Rect2D a
rectUnion r1 r2
  = rectBetween (pt (min (rectLeft r1) (rectLeft r2)) (min (rectTop r1) (rectTop r2)))
         (pt (max (rectRight r1) (rectRight r2)) (max (rectBottom r1) (rectBottom r2)))

rectUnions :: (Num a, Ord a) => [Rect2D a] -> Rect2D a
rectUnions []
  = rectZero
rectUnions (r:rs)
  = foldr rectUnion r rs

-- | The intersection between two rectangles.
rectOverlap :: (Num a, Ord a) => Rect2D a -> Rect2D a -> Rect2D a
rectOverlap r1 r2
  | rectOverlaps r1 r2  = rectBetween (pt (max (rectLeft r1) (rectLeft r2)) (max (rectTop r1) (rectTop r2)))
                               (pt (min (rectRight r1) (rectRight r2)) (min (rectBottom r1) (rectBottom r2)))
  | otherwise           = rectZero


{-----------------------------------------------------------------------------------------
  Color
-----------------------------------------------------------------------------------------}
-- | An abstract data type to define colors.
--
--   Note: Haddock 0.8 and 0.9 doesn't support GeneralizedNewtypeDeriving. So, This class
--   doesn't have 'IArray' class's unboxed array instance now. If you want to use this type
--   with unboxed array, you must write code like this.
--
-- > {-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses #-}
-- > import Graphics.UI.WXCore.WxcTypes
-- > ...
-- > deriving instance IArray UArray Color
--
--   We can't derive 'MArray' class's unboxed array instance this way. This is a bad point
--   of current 'MArray' class definition.
--
newtype Color = Color Int
--newtype Color = Color Word 
              deriving (Eq, Typeable) -- , IArray UArray) 

-- Crashes UHC
--instance Show Color where
--  showsPrec d c
--    = showParen (d > 0) (showString "rgba(" . shows (colorRed   c) .
--                          showChar   ','    . shows (colorGreen c) .
--                          showChar   ','    . shows (colorBlue  c) .
--                          showChar   ','    . shows (colorAlpha c) .
--                          showChar   ')' )

-- | Create a color from a red\/green\/blue triple.
colorRGB :: (Integral a) => a -> a -> a -> Color
colorRGB r g b = Color (shiftL (fromIntegral r) 24 .|. shiftL (fromIntegral g) 16 .|. shiftL (fromIntegral b) 8 .|. 255)

-- | Create a color from a red\/green\/blue triple.
rgb :: (Integral a) => a -> a -> a -> Color
rgb r g b = colorRGB r g b

-- | Create a color from a red\/green\/blue\/alpha quadruple.
colorRGBA :: (Integral a) => a -> a -> a -> a -> Color
colorRGBA r g b a = Color (shiftL (fromIntegral r) 24 .|. shiftL (fromIntegral g) 16 .|. shiftL (fromIntegral b) 8 .|. (fromIntegral a))

-- | Create a color from a red\/green\/blue\/alpha quadruple.
rgba :: (Integral a) => a -> a -> a -> a -> Color
rgba r g b a = colorRGBA r g b a


-- | Return an 'Int' where the three least significant bytes contain
-- the red, green, and blue component of a color.
intFromColor :: Color -> Int
intFromColor rgb
  = let r = colorRed rgb
        g = colorGreen rgb
        b = colorBlue rgb
    in (shiftL (fromIntegral r) 16 .|. shiftL (fromIntegral g) 8 .|. b)

-- | Set the color according to an rgb integer. (see 'rgbIntFromColor').
colorFromInt :: Int -> Color
colorFromInt rgb
  = let r = (shiftR rgb 16) .&. 0xFF
        g = (shiftR rgb 8) .&. 0xFF
        b = rgb .&. 0xFF
    in colorRGB r g b

-- | Return an 'Num' class's numeric representation where the three
-- least significant the red, green, and blue component of a color.
fromColor :: (Num a) => Color -> a
fromColor (Color rgb)
  = fromIntegral rgb

-- | Set the color according to 'Integral' class's numeric representation.
-- (see 'rgbaIntFromColor').
toColor :: (Integral a) => a -> Color
toColor
  = Color . fromIntegral

-- marshalling 1
-- | Returns a red color component
colorRed   :: (Num a) => Color -> a
colorRed   (Color rgba) = fromIntegral ((shiftR rgba 24) .&. 0xFF)

-- | Returns a green color component
colorGreen :: (Num a) => Color -> a
colorGreen (Color rgba) = fromIntegral ((shiftR rgba 16) .&. 0xFF)

-- | Returns a blue color component
colorBlue  :: (Num a) => Color -> a
colorBlue  (Color rgba) = fromIntegral ((shiftR rgba 8) .&. 0xFF)

-- | Returns a alpha channel component
colorAlpha  :: (Num a) => Color -> a
colorAlpha  (Color rgba) = fromIntegral (rgba .&. 0xFF)

{-----------------------------------------------------------------------------------------
 Default colors.
-----------------------------------------------------------------------------------------}
black, darkgrey, dimgrey, mediumgrey, grey, lightgrey, white :: Color
red, green, blue :: Color
cyan, magenta, yellow :: Color

black     = colorRGB 0x00 0x00 0x00
darkgrey  = colorRGB 0x2F 0x2F 0x2F
dimgrey   = colorRGB 0x54 0x54 0x54
mediumgrey= colorRGB 0x64 0x64 0x64
grey      = colorRGB 0x80 0x80 0x80
lightgrey = colorRGB 0xC0 0xC0 0xC0
white     = colorRGB 0xFF 0xFF 0xFF

red       = colorRGB 0xFF 0x00 0x00
green     = colorRGB 0x00 0xFF 0x00
blue      = colorRGB 0x00 0x00 0xFF

yellow    = colorRGB 0xFF 0xFF 0x00
magenta   = colorRGB 0xFF 0x00 0xFF
cyan      = colorRGB 0x00 0xFF 0xFF


{--------------------------------------------------------------------------
  System colors
--------------------------------------------------------------------------}
-- | System Colors.
data SystemColor
  = ColorScrollBar        -- ^ The scrollbar grey area.  
  | ColorBackground       -- ^ The desktop colour.  
  | ColorActiveCaption    -- ^ Active window caption.  
  | ColorInactiveCaption  -- ^ Inactive window caption.  
  | ColorMenu             -- ^ Menu background.  
  | ColorWindow           -- ^ Window background.  
  | ColorWindowFrame      -- ^ Window frame.  
  | ColorMenuText         -- ^ Menu text.  
  | ColorWindowText       -- ^ Text in windows.  
  | ColorCaptionText      -- ^ Text in caption, size box and scrollbar arrow box.  
  | ColorActiveBorder     -- ^ Active window border.  
  | ColorInactiveBorder   -- ^ Inactive window border.  
  | ColorAppWorkspace     -- ^ Background colour MDI -- ^applications.  
  | ColorHighlight        -- ^ Item(s) selected in a control.  
  | ColorHighlightText    -- ^ Text of item(s) selected in a control.  
  | ColorBtnFace          -- ^ Face shading on push buttons.  
  | ColorBtnShadow        -- ^ Edge shading on push buttons.  
  | ColorGrayText         -- ^ Greyed (disabled) text.  
  | ColorBtnText          -- ^ Text on push buttons.  
  | ColorInactiveCaptionText -- ^ Colour of text in active captions.  
  | ColorBtnHighlight     -- ^ Highlight colour for buttons (same as 3DHILIGHT).  
  | Color3DDkShadow       -- ^ Dark shadow for three-dimensional display elements.  
  | Color3DLight          -- ^ Light colour for three-dimensional display elements.  
  | ColorInfoText         -- ^ Text colour for tooltip controls.  
  | ColorInfoBk           -- ^ Background colour for tooltip controls.  
  | ColorDesktop          -- ^ Same as BACKGROUND.  
  | Color3DFace           -- ^ Same as BTNFACE.  
  | Color3DShadow         -- ^ Same as BTNSHADOW.  
  | Color3DHighlight      -- ^ Same as BTNHIGHLIGHT.  
  | Color3DHilight        -- ^ Same as BTNHIGHLIGHT.  
  | ColorBtnHilight       -- ^ Same as BTNHIGHLIGHT.  

instance Enum SystemColor where
  toEnum i
    = error "Graphics.UI.WXCore.Types.SytemColor.toEnum: can not convert integers to system colors."

  fromEnum systemColor
    = case systemColor of
        ColorScrollBar        -> wxSYS_COLOUR_SCROLLBAR
        ColorBackground       -> wxSYS_COLOUR_BACKGROUND
        ColorActiveCaption    -> wxSYS_COLOUR_ACTIVECAPTION
        ColorInactiveCaption  -> wxSYS_COLOUR_INACTIVECAPTION
        ColorMenu             -> wxSYS_COLOUR_MENU
        ColorWindow           -> wxSYS_COLOUR_WINDOW
        ColorWindowFrame      -> wxSYS_COLOUR_WINDOWFRAME
        ColorMenuText         -> wxSYS_COLOUR_MENUTEXT
        ColorWindowText       -> wxSYS_COLOUR_WINDOWTEXT
        ColorCaptionText      -> wxSYS_COLOUR_CAPTIONTEXT
        ColorActiveBorder     -> wxSYS_COLOUR_ACTIVEBORDER
        ColorInactiveBorder   -> wxSYS_COLOUR_INACTIVEBORDER
        ColorAppWorkspace     -> wxSYS_COLOUR_APPWORKSPACE 
        ColorHighlight        -> wxSYS_COLOUR_HIGHLIGHT
        ColorHighlightText    -> wxSYS_COLOUR_HIGHLIGHTTEXT
        ColorBtnFace          -> wxSYS_COLOUR_BTNFACE
        ColorBtnShadow        -> wxSYS_COLOUR_BTNSHADOW
        ColorGrayText         -> wxSYS_COLOUR_GRAYTEXT
        ColorBtnText          -> wxSYS_COLOUR_BTNTEXT
        ColorInactiveCaptionText -> wxSYS_COLOUR_INACTIVECAPTIONTEXT
        ColorBtnHighlight     -> wxSYS_COLOUR_BTNHIGHLIGHT
        Color3DDkShadow       -> wxSYS_COLOUR_3DDKSHADOW
        Color3DLight          -> wxSYS_COLOUR_3DLIGHT
        ColorInfoText         -> wxSYS_COLOUR_INFOTEXT
        ColorInfoBk           -> wxSYS_COLOUR_INFOBK
        ColorDesktop          -> wxSYS_COLOUR_DESKTOP
        Color3DFace           -> wxSYS_COLOUR_3DFACE
        Color3DShadow         -> wxSYS_COLOUR_3DSHADOW
        Color3DHighlight      -> wxSYS_COLOUR_3DHIGHLIGHT
        Color3DHilight        -> wxSYS_COLOUR_3DHILIGHT
        ColorBtnHilight       -> wxSYS_COLOUR_BTNHILIGHT

      
-- | Convert a system color to a color. 
colorSystem :: SystemColor -> Color
colorSystem systemColor
  = unsafePerformIO $ 
    wxcSystemSettingsGetColour (fromEnum systemColor)

wxcSystemSettingsGetColour :: Int -> IO Color
wxcSystemSettingsGetColour = error "not implemented"

wxSIZE_USE_EXISTING = error "wxSIZE_USE_EXISTING not implemented"
wxNO_FULL_REPAINT_ON_RESIZE = error "wxNO_FULL_REPAINT_ON_RESIZE not implemented"
wxCLIP_CHILDREN = error "wxCLIP_CHILDREN not implemented"
wxRESIZE_BORDER = error "wxRESIZE_BORDER not implemented"
wxCLOSE_BOX = error "wxCLOSE_BOX not implemented"
wxMINIMIZE_BOX = error "wxMINIMIZE_BOX not implemented"
wxMAXIMIZE_BOX = error "wxMAXIMIZE_BOX not implemented"
wxSYS_COLOUR_SCROLLBAR = error "wxSYS_COLOUR_SCROLLBAR not implemented"
wxSYS_COLOUR_BACKGROUND = error "wxSYS_COLOUR_BACKGROUND not implemented"
wxSYS_COLOUR_ACTIVECAPTION = error "wxSYS_COLOUR_ACTIVECAPTION not implemented"
wxSYS_COLOUR_INACTIVECAPTION = error "wxSYS_COLOUR_INACTIVECAPTION not implemented"
wxSYS_COLOUR_MENU = error "wxSYS_COLOUR_MENU not implemented"
wxSYS_COLOUR_WINDOW = error "wxSYS_COLOUR_WINDOW not implemented"
wxSYS_COLOUR_WINDOWFRAME = error "wxSYS_COLOUR_WINDOWFRAME not implemented"
wxSYS_COLOUR_MENUTEXT = error "wxSYS_COLOUR_MENUTEXT not implemented"
wxSYS_COLOUR_WINDOWTEXT = error "wxSYS_COLOUR_WINDOWTEXT not implemented"
wxSYS_COLOUR_CAPTIONTEXT = error "wxSYS_COLOUR_CAPTIONTEXT not implemented"
wxSYS_COLOUR_ACTIVEBORDER = error "wxSYS_COLOUR_ACTIVEBORDER not implemented"
wxSYS_COLOUR_INACTIVEBORDER = error "wxSYS_COLOUR_INACTIVEBORDER not implemented"
wxSYS_COLOUR_APPWORKSPACE  = error "wxSYS_COLOUR_APPWORKSPACE  not implemented"
wxSYS_COLOUR_HIGHLIGHT = error "wxSYS_COLOUR_HIGHLIGHT not implemented"
wxSYS_COLOUR_HIGHLIGHTTEXT = error "wxSYS_COLOUR_HIGHLIGHTTEXT not implemented"
wxSYS_COLOUR_BTNFACE = error "wxSYS_COLOUR_BTNFACE not implemented"
wxSYS_COLOUR_BTNSHADOW = error "wxSYS_COLOUR_BTNSHADOW not implemented"
wxSYS_COLOUR_GRAYTEXT = error "wxSYS_COLOUR_GRAYTEXT not implemented"
wxSYS_COLOUR_BTNTEXT = error "wxSYS_COLOUR_BTNTEXT not implemented"
wxSYS_COLOUR_INACTIVECAPTIONTEXT = error "wxSYS_COLOUR_INACTIVECAPTIONTEXT not implemented"
wxSYS_COLOUR_BTNHIGHLIGHT = error "wxSYS_COLOUR_BTNHIGHLIGHT not implemented"
wxSYS_COLOUR_3DDKSHADOW = error "wxSYS_COLOUR_3DDKSHADOW not implemented"
wxSYS_COLOUR_3DLIGHT = error "wxSYS_COLOUR_3DLIGHT not implemented"
wxSYS_COLOUR_INFOTEXT = error "wxSYS_COLOUR_INFOTEXT not implemented"
wxSYS_COLOUR_INFOBK = error "wxSYS_COLOUR_INFOBK not implemented"
wxSYS_COLOUR_DESKTOP = error "wxSYS_COLOUR_DESKTOP not implemented"
wxSYS_COLOUR_3DFACE = error "wxSYS_COLOUR_3DFACE not implemented"
wxSYS_COLOUR_3DSHADOW = error "wxSYS_COLOUR_3DSHADOW not implemented"
wxSYS_COLOUR_3DHIGHLIGHT = error "wxSYS_COLOUR_3DHIGHLIGHT not implemented"
wxSYS_COLOUR_3DHILIGHT = error "wxSYS_COLOUR_3DHILIGHT not implemented"
wxSYS_COLOUR_BTNHILIGHT = error "wxSYS_COLOUR_BTNHILIGHT not implemented"

wxEVT_TIMER :: EventType
wxEVT_TIMER = 1

wxEVT_PAINT :: EventType
wxEVT_PAINT = 2

wxEVT_CHAR :: EventType
wxEVT_CHAR = 3