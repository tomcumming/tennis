module Main where

import Math
import Graphics.Gloss
import Data.Maybe (mapMaybe, fromJust)

(width, height) = (800, 600)
pixelsPerMeter = 100

p1 = (1, 2)
p2 = (4, 3)
p3 = (7, 1)

g = 9.81

translateV2 (x, y) = Translate x y

para = parabola p1 p2 p3
v = parabolaInitialVel para p1 g
theta = atan2 (snd v) (fst v)
speed = mag v

xps = [p1x, (p1x + 0.1) .. fst p3] where p1x = fst p1

paraPath :: Path
paraPath = map 
  (\x -> (x, parabolaHeightAt para x)) 
  xps

shotPath :: Path
shotPath = map
  (\x -> (x, fromJust $ heightAt p1 x theta speed g))
  xps

draw = 
  Translate (width / (-2)) (height / (-2)) 
  $ Scale (pixelsPerMeter) (pixelsPerMeter) 
  $ Pictures
    ( [ (translateV2 p1 $ Color white (Circle 0.1))
      , (translateV2 p2 $ Color red (Circle 0.1))
      , (translateV2 p3 $ Color blue (Circle 0.1))
      ]
      ++ [Color (makeColor 1 0 0 0.5) $ Line paraPath]
      ++ [Color (makeColor 0 0 1 0.5) $ Line shotPath]
    )

main = do
  putStrLn "Testing parabola math:"
  putStrLn "Should show a purple curve through all points"
  display 
    (InWindow "parabola" (floor width, floor height) (50, 50)) 
    black
    draw
