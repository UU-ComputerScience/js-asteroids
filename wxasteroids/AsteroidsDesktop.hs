module Main where

import Graphics.UI.WX
import System.Random

height   :: Int
height   = 300 

width    :: Int
width    = 300 

diameter :: Int
diameter = 24 

chance   :: Double 
chance   = 0.1

asteroids :: IO () 
asteroids = 
  do
    g      <- getStdGen 
    vrocks <- varCreate $ randomRocks g
    vship  <- varCreate $ div width 2

    -- Using a window causes segfault...
    --w <- window objectNull [area := rect (pt 0 0) (sz width height)]
    f  <- frame   [ area := rect (pt 0 0) (sz width height)]

    t  <- timer f [ interval   := 50
                  , on command := advance vrocks f
                  ]
    
    set f [
             on paint    := draw vrocks vship 
           , on leftKey  := varUpdate vship (\x -> max 0     (x - 5)) >> return ()
           , on rightKey := varUpdate vship (\x -> min width (x + 5)) >> return ()
           , on (charKey 'q') := set t [interval :~ \i -> i * 2]
           , on (charKey 'w') := set t [interval :~ \i -> max 10 (div i 2)]
          ]

--advance :: (Textual w, Paint w1) => w -> Var [[a]] -> w1 -> IO ()
advance :: (Paint w) => Var [[a]] -> w -> IO ()
advance vrocks f =
  do 
    (r : rs) <- varGet vrocks 
    varSet vrocks rs
    repaint f 

randomRocks :: RandomGen g => g -> [[Point]] 
randomRocks g = flatten [] (map fresh (randoms g)) 

flatten :: [[a]] -> [[[a]]] -> [[a]]
flatten rocks (t : ts) = 
  let now   = map head rocks 
      later = filter (not . null) (map tail rocks) 
  in now : flatten (t ++ later) ts 
flatten rocks [] = error "Empty rocks list not expected in function flatten"

fresh :: Double -> [[Point2 Int]]
fresh r 
  | r > chance = [] 
  | otherwise  = [track (floor (fromIntegral width * r / chance))] 

track :: Int -> [Point2 Int]
track x = [point x (y - diameter) | y <- [0, 6 .. height + 2 * diameter]] 

--draw :: Var [[Point2 Int]] -> Var Int -> DC a -> b -> IO ()
draw :: Var [[Point2 Int]] -> Var Int -> DC a -> b -> IO ()
draw vrocks vship dc _view =
  do
    rocks <- varGet vrocks 
    x     <- varGet vship 
    let
      shipLocation = point x (height - 2 * diameter)
      positions    = head rocks
      collisions   = map (collide shipLocation) positions
      
    drawShip dc shipLocation
    mapM (drawRock dc) (zip positions collisions) 
    --when (or collisions) (play explode) 
    when (or collisions) (return ())

collide :: Point2 Int -> Point2 Int -> Bool
collide pos0 pos1 = 
  let distance = vecLength (vecBetween pos0 pos1) 
  in distance <= fromIntegral diameter

--drawShip :: DC a -> Point -> IO ()
drawShip :: DC a -> Point -> IO ()
drawShip dc pos = drawBitmap dc ship pos True [] 

--drawRock :: DC a -> (Point, Bool) -> IO ()
drawRock :: DC a -> (Point, Bool) -> IO ()
drawRock dc (pos, collides)= 
  let rockPicture = if collides then burning else rock
  in do drawBitmap dc rockPicture pos True []

rock    :: Bitmap ()
rock    = bitmap "resources/rock.ico"

burning :: Bitmap ()
burning = bitmap "resources/burning.ico"

ship    :: Bitmap ()
ship    = bitmap "resources/ship.ico"

main :: IO ()
main = start asteroids

