{- Copied from OOHaskell source code -}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE UndecidableInstances#-}
{-# LANGUAGE OverlappingInstances#-}
{-# LANGUAGE DatatypeContexts #-}
module Examples.Print
 (
   printLn, 
   (<<)
 )
   where

import Prelude hiding (print)
--import Data.HList.HListPrelude


-- The type-class for printing

class PrintType x
 where
  print :: x -> IO ()


-- Printable composites

data (PrintType x, PrintType y) => PrintPair x y = PrintPair x y


-- Compose printable expressions (aka daisy chaining)

infixl 7 << 
(<<) ::  (PrintType x, PrintType y) => x -> y -> PrintPair x y 
x << y = PrintPair x y 


-- Strings are printable right away 

instance PrintType String
 where
  print = putStr


-- IO computations are computed and then printed with the help of shown

instance Show x => PrintType (IO x)
 where
  print x = x >>= putStr . show 


-- Printing compound expressions

instance (PrintType x, PrintType y) => PrintType (PrintPair x y)
 where
  print (PrintPair x y) = print x >> print y


-- Printing the rest

instance Show x => PrintType x
 where
  print = putStr . show


-- Print expression and begin a new line

printLn x = print x >> putStr "\n" 
