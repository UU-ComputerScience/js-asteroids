{-# LANGUAGE ExistentialQuantification #-}
module HomoList where

data Shape a
data Rectangle a
data Circle a

rect = undefined :: Shape (Rectangle ())

circ = undefined :: Shape (Circle ())

-- doesn't work
--homoExt = [rect, circ] :: [exists a. Shape a]

homoExt' :: [exists a. Shape a]
homoExt' = [rect, circ]

foo :: Shape a -> Bool
foo _ = True

test = map foo homoExt'
