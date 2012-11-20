module Language.UHC.JS.ECMA.Array where

import Language.UHC.JS.Types
--import UHC.Array
--import UHC.Base as B
--import UHC.BoxArray

--foreign import js "%1.length"
--  lengthJSArray :: JSArray x -> Int

--{- foreign import js "%1.toString"-}
--  {- toString :: JSArray x -> JSString-}
--{- foreign import js "%1.toLocaleString" toLocaleString :: JSArray x -> JSString-}

---- TODO: How do we deal with the fact that this fun can accept an arbitrary
---- number of arguments? How do we deal with non-arrays being passed? Do we need
---- to specified a ToJS constraint on those? Or can we let JS figure out what to
---- do?
foreign import js "%1.concat(%*)"
  concat :: JSArray x -> JSArray x -> JSArray x

--foreign import js "%1.concat(%*)"
--  concat2 :: JSArray x -> JSArray x -> JSArray x -> JSArray x

--foreign import js "%1.concat(%*)"
--  concat3 :: JSArray x -> JSArray x -> JSArray x -> JSArray x -> JSArray x
---- etc.

--{- foo = concat' arr1 (JSNArgs [arr2, arr3, arr4])-}
--{- foo = concat' arr1 [arr2, ]-}
---- TODO: The ECMA standard specifies that the separator argument is optional
---- and a comma will be used if no separator is specified. How do we want to
---- model optional arguments? Do we want to make separate imports, each with
---- different arguments? Or do we want to use Maybes? Or do we want some monadic
---- construct like Roman suggested? We might also just want to make a couple of
---- alternative imports. It'd be easiest for functions with a small number of
---- optional arguments. Funs with more optional arguments still require some
---- thought though.
--foreign import js "%1.join" join  :: JSArray x -> S.JSString
--foreign import js "%1.join(%*)" join' :: JSArray x -> S.JSString -> S.JSString

---- TODO: Do we want this to be in IO? We're mutating the array here...
---- head/tail teruggeven in tupletje, in IO
--foreign import js "%1.pop"
--  pop :: JSArray x -> IO x

---- TODO: Again we are stuck with the n-argument problem
---- | Push a new element onto an array. ECMA specifies that the new length is
---- returned.
--foreign import js "%1.push(%*)"
--  push :: JSArray x -> x -> Int

--foreign import js "%1.push(%*)"
--  push2 :: JSArray x -> x -> x -> Int

--foreign import js "%1.reverse"
--  reverse :: JSArray x -> JSArray x

--foreign import js "%1.shift"
--  shift :: JSArray x -> x

--foreign import js "%1.slice(%*)"
--  slice :: JSArray x -> Int -> Int -> JSArray x

--foreign import js "%1.sort"
--  sort :: JSArray x -> JSArray x

---- TODO: The sort function is optioanl
---- TODO: Can we pass a function in this way? Or do we need to peek at the C FFI for wrapper ideas?
---- TODO: Again, do we want to be in IO? The callback could be anything, technically.
--foreign import js "%1.sort(%*)"
--  sort' :: JSArray x -> (x -> x -> Int) -> JSArray x

---- TODO: Yet again, the n-argument problem.
---- TODO: "array starting at array index start" can we assume array indices are always numeric? I think so....
---- TODO: Maybe we should model the n-arguments as a list? Or as some special NArg type, which contains a list?
----  newtype NArgs a = NArgs [a]
--foreign import js "%1.splice(%*)"
--  splice :: JSArray x -> Int -> Int -> JSArray x

--foreign import js "%1.splice(%*)"
--  splice2 :: JSArray x -> Int -> Int -> x -> JSArray x

---- TODO: n-arg
--foreign import js "%1.unshift(%*)"
--  unshift :: JSArray x -> x -> Int

--foreign import js "%1.unshift(%*)"
--  unshift2 :: JSArray x -> x -> x -> Int


---- TODO: The JS fun always returns an int. A -1 for not found and some other
---- n>=0 otherwise. We probably need Haskell wrapper functions for these things.
---- We need to come up with some naming scheme for wrappers and their underlying
---- functions.
----
---- I maintain the following naming scheme for these indexOf functions:
---- - When there are few optional arguments (e.g., n < 5), create separate functions for each optional argument
---- - These function names get a ' appended
---- - Since the lookup can fail, we want a Maybe type as a return value, hence we wrap the import
---- - Since we're wrapping, we're naming the import after the JS function name, prefixed with an underscore
---- - The HS function then just gets the JS function name (possibly with ') and calls the import
--foreign import js "%1.indexOf(%*)"
--  _indexOf  :: JSArray x -> x -> Int

--foreign import js "%1.indexOf(%*)"
--  _indexOf' :: JSArray x -> x -> Int -> Int

--indexOf :: JSArray x -> x -> Maybe Int
--indexOf a x = mkIdxRes $ _indexOf a x

--indexOf' :: JSArray x -> x -> Int -> Maybe Int
--indexOf' a x i = mkIdxRes $ _indexOf' a x i


---- TODO: Same problems as previous one
--foreign import js "%1.lastIndexOf(%*)"
--  _lastIndexOf  :: JSArray x -> x -> Int

--foreign import js "%1.lastIndexOf(%*)"
--  _lastIndexOf' :: JSArray x -> x -> Int -> Int

--lastIndexOf :: JSArray x -> x -> Maybe Int
--lastIndexOf a x = mkIdxRes $ _lastIndexOf a x

--lastIndexOf' :: JSArray x -> x -> Int -> Maybe Int
--lastIndexOf' a x i = mkIdxRes $ _lastIndexOf' a x i

--foreign import js "%1.every(%*)"
--  every :: JSArray x -> (x -> Int -> JSArray x -> Bool) -> Bool

---- TODO: the 'a' is supposed to be the this value for the callback. Maybe we should
---- create a JSObject type which can be passed here?
--foreign import js "%1.every(%*)"
--  every' :: JSArray x -> (x -> Int -> JSArray x -> Bool) -> a -> Bool

---- TODO: Similar problems to above
--foreign import js "%1.some(%*)"
--  some :: JSArray x -> (x -> Int -> JSArray x -> Bool) -> Bool

--foreign import js "%1.some(%*)"
--  some' :: JSArray x -> (x -> Int -> JSArray x -> Bool) -> a -> Bool

---- TODO: Similar problems to above
--foreign import js "%1.forEach(%*)"
--  forEach :: JSArray x -> (x -> Int -> JSArray x -> ()) -> ()

--foreign import js "%1.forEach(%*)"
--  forEach' :: JSArray x -> (x -> Int -> JSArray x -> ()) -> a -> ()

---- TODO: Similar problems to above
--foreign import js "%1.map(%*)"
--  map :: JSArray x -> (x -> Int -> JSArray x -> y) -> JSArray y

--foreign import js "%1.map(%*)"
--  map' :: JSArray x -> (x -> Int -> JSArray x -> y) -> a -> JSArray y

--foreign import js "%1.filter(%*)"
--  filter :: JSArray x -> (x -> Int -> JSArray x -> Bool) -> JSArray x

--foreign import js "%1.filter(%*)"
--  filter' :: JSArray x -> (x -> Int -> JSArray x -> Bool) -> a -> JSArray x

--foreign import js "%1.reduce(%*)"
--  reduce :: JSArray x -> (x -> x -> Int -> JSArray x -> y) -> y

--foreign import js "%1.reduce(%*)"
--  reduce' :: JSArray x -> (x -> x -> Int -> JSArray x -> y) -> y -> y

--foreign import js "%1.reduceRight(%*)"
--  reduceRight  :: JSArray x -> (x -> x -> Int -> JSArray x -> y) -> y

--foreign import js "%1.reduceRight(%*)"
--  reduceRight' :: JSArray x -> (x -> x -> Int -> JSArray x -> y) -> y -> y

