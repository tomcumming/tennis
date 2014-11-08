module Main where

import Math
import Graphics.Gloss
import Data.Maybe (mapMaybe)

(width, height) = (800, 600)
pixelsPerMeter = 100

type World = 
  ( V2 -- start
  , V2 -- target
  , Scalar -- gravity
  , Scalar -- power
  )

testWorld :: World
testWorld = ((1, 1), (7, 2), (9.81), 9)

makePaths :: World -> [Path]
makePaths (s@(sx, _), e@(ex, _), g, v) = map (zip xs) $ mapMaybe toHeights as
  where
    as = requiredToHit s e v g
    xs = [sx, (sx + 0.1) .. ex]
    toHeights a = sequence 
      $ map (\x -> heightAt s x a v g) xs

draw w@((startx, starty), (targetx, targety), g, v) = 
  Translate (width / (-2)) (height / (-2)) 
  $ Scale (pixelsPerMeter) (pixelsPerMeter) 
  $ Pictures
    ( [ (Translate startx starty $ Color white (Circle 0.1))
      , (Translate targetx targety $ Color red (Circle 0.1))
      ]
      ++ zipWith Color [green, blue] (map Line (makePaths w))
    )

main = do
  putStrLn "Testing shot trajectory solving:"
  putStrLn "Should show one or more curves between the circled points"
  display 
    (InWindow "test" (floor width, floor height) (50, 50)) 
    black
    (draw testWorld)
