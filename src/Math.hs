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

mag :: V2 -> Scalar
mag (x, y) = sqrt (x * x + y * y)

{-
Find the height at x when a projectile is fired at angle theta from starting
position (sx, sy) with an initial velocity of v and gravity acceleration speed
of g
(g should be positive)
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
(g should be positive)
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

-- Has the form (a, b, c) where y = a * x * x + b * x + c
type Parabola = (Scalar, Scalar, Scalar)

{-
Find the parabola that passes through three points
-}
parabola :: V2 -> V2 -> V2 -> Parabola
parabola (x1, y1) (x2, y2) (x3, y3) = (a, b, c)
  where
    d = (x1 - x2) * (x1 - x3) * (x2 - x3)
    a = (x3 * (y2 - y1) + x2 * (y1 - y3) + x1 * (y3 - y2)) / d
    b = (x3 ** 2 * (y1 - y2) + x2 ** 2 * (y3 - y1) + x1 ** 2 * (y2 - y3)) / d
    c = (x2 * x3 * (x2 - x3) * y1 + x3 * x1 * (x3 - x1) 
      * y2 + x1 * x2 * (x1 - x2) * y3) / d

{-
Get the required initial velocity for a projectile to follow parabola when
given a starting position and gravity acceleration speed
(g should be positive)
-}
parabolaInitialVel :: Parabola -> V2 -> Scalar -> V2
parabolaInitialVel (a, b, c) (sx, sy) g = (vx, vy)
  where
    vx = sqrt (-g / (2 * a))
    vy = 2 * a * vx * sx + b * vx

{-
Finds y when given x and a parabola
-}
parabolaHeightAt :: Parabola -> Scalar -> Scalar
parabolaHeightAt (a, b, c) x = a * x * x + b * x + c

