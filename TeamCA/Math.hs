module TeamCA.Math where

distOrigin = dist (0, 0)

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x0, y0) (x1, y1) = sqrt $ (x1 - x0) ^ 2 + (y1 - y0) ^ 2

onVector2 op (x0, y0) (x1, y1) = (x0 `op` x1, y0 `op` y1)

toPolar :: (Double, Double) -> (Double, Double)
toPolar (x, y) = (radius, degrees) 
    where 
        radius = distOrigin (x, y)
        degrees 
            | x == 0 && y== 0      = 0
            | x >= 0                = asin $ y / radius
            | x < 0                 = asin (y / radius) + pi

fromPolar :: (Double, Double) -> (Double, Double)
fromPolar (radius, degrees) = (x, y)
    where 
        x = radius * (cos degrees)
        y = radius * (sin degrees)

type Point = (Double, Double)
type Vector = Point
 
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)
 
-- Compute the normalized tangent vector from two points
normalizedVector :: Point -> Point -> Vector
normalizedVector (x1, y1) (x2, y2) = (xv / l, yv / l)
    where
      xv = x2 - x1
      yv = y2 - y1
      l = distance (0, 0) (xv, yv)
          
mu :: Double
mu = 6.67428e-11 * 6.0e24
 
times :: Double -> Vector -> Vector
times v (x, y) = (v*x, v*y)
 
-- vec is the result of computing normalizedVector on the first two points that
-- were observed (don't fire the rockets before observing!)
delta1 :: Double -> Double -> Vector -> Vector
delta1 r1 r2 vec = (lhs*rhs) `times` vec
    where
      lhs = sqrt (mu / r1)
      rhs = (sqrt (2 * r2 / (r1 + r2))) - 1

mu8 = mu * 8.0
hohTime radius1 radius2 = pi * sqrt (((radius1 + radius2) ^ 3) / mu8)
 
-- same note as above
delta2 :: Double -> Double -> Vector -> Vector
delta2 r1 r2 vec = (lhs*rhs) `times` vec
    where
      lhs = sqrt (mu / r2)
      rhs = 1 - (sqrt (2 * r1 / (r1 + r2)))
