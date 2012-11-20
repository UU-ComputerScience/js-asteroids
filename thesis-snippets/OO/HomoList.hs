{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module HomoList where

data Shape a
data Rectangle a
data Circle a

rect = undefined :: Shape (Rectangle ())

circ = undefined :: Shape (Circle ())

-- Ty error
--test = [rect, circ] :: [Shape a]

-- Existentials

data ShapeExt = forall a. ShapeExt (Shape a)

--homoExt :: [exists a. Shape a]
--homoExt = [rect, circ]

homoExt = [ShapeExt rect, ShapeExt circ] :: [ShapeExt]

-- Either

homoEither = [Left rect, Right circ]
