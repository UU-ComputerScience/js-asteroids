{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
{-|	Module      :  Types
	Copyright   :  (c) Daan Leijen 2003
	License     :  wxWindows

	Maintainer  :  wxhaskell-devel@lists.sourceforge.net
	Stability   :  provisional
	Portability :  portable

Basic types.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Types (
   module Graphics.UI.WXCore.Types
  ,module Graphics.UI.WX.Types
) where

import Graphics.UI.WXCore.Types
--import Graphics.UI.WXCore.Draw
import Graphics.UI.WXCore.Event

-- | Inverse application, i.e. @feed x f@ = @f x@.
feed :: a -> (a -> b) -> b
feed x f
  = f x

-- | Composed Inverse application, i.e. @feed2 x y f@ = @f x y@.
feed2 :: a -> b -> (a -> b -> c) -> c
feed2 x y f
  = f x y

-- | Data types that can be represented through a bit mask. Only the @assocBitMask@ method
-- is required for a new instance.
class Eq b => BitMask b where
  -- | Give the association between the constructors and the bits. If a constructor
  -- corresponds to no bits set, it should come as the last element.
  assocBitMask :: [(b,Int)]

  -- | Convert to a bitmask
  toBitMask    :: b -> Int
  -- | Convert from a bitmask
  fromBitMask  :: Int -> b
  -- | Set the correct bits corresponding to a constructor in a mask.
  setBitMask :: b -> Int -> Int

  toBitMask x
    = case lookup x assocBitMask of
        Just m  -> m
        Nothing -> 0

  fromBitMask i
    = walk assocBitMask
    where
      walk []         = error "Graphics.UI.WX.Types.fromBitMask: empty list"
      walk [(x,0)]    = x
      walk ((x,m):xs) | bitsSet m i = x
                      | otherwise   = walk xs

  setBitMask x i
    = i .-. (bits (map snd (assocBitMask::[(b,Int)]))) .+. toBitMask x


-- | Create a bitmask from a list of types.
mask :: BitMask b => [b] -> Int
mask xs
  = foldr (.+.) 0 (map toBitMask xs)