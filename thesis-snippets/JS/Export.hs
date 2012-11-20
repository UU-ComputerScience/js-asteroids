module Export where

minus :: Int -> Int -> Int
minus x y = x - y

foreign export js "minus"
   minus :: Int -> Int -> Int
