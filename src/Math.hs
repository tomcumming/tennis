{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Math where

import Data.List (nub)

type Scalar = Float
type V2 = (Scalar, Scalar)

instance Num V2 where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2)
  negate (x, y) = (negate x, negate y)
  fromInteger n = (fromInteger n, fromInteger n)
  abs = undefined
  signum = undefined

{-
Find the height at x when a projectile is fired at angle theta from starting
position (sx, sy) with an initial velocity of v and gravity acceleration speed
of g
-}
heightAt :: V2 -> Scalar -> Scalar -> Scalar -> Scalar -> Maybe Scalar
heightAt (sx, sy) x theta v g
  | isNaN y   = Nothing
  | otherwise = Just y
  where
    rx = x - sx
    y = sy + rx * tan theta - ((g * rx * rx) / (2 * (v * cos theta) ** 2))

{-
Given a start position, target position, an initial velocity and gravity
acceleration speed, calculate the angles which will hit the target 
(will return 0, 1 or 2 valid results)
-}
requiredToHit :: V2 -> V2 -> Scalar -> Scalar -> [Scalar]
requiredToHit start target v g = nub $ filter (not . isNaN) [a1, a2]
  where
    (rx, ry) = target - start
    a1 = atan ((v * v + sqrt (v ** 4 - g * (g * rx * rx + 2 * ry * v * v)))
      / (g * rx))
    a2 = atan ((v * v - sqrt (v ** 4 - g * (g * rx * rx + 2 * ry * v * v)))
      / (g * rx))

