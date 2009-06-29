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

normalize :: Vector -> Vector
normalize (x, y) = (x / mag, y / mag)
    where
      mag = distOrigin (x, y)
          
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

-- The fields are 
data Arc = Arc Vector Vector Double
    deriving (Ord, Eq, Show)

-- The fields are position, speed. Speed is positive for clockwise rotation,
-- negative for counter-clockwise rotation
data Course = Course Point Double

-- Calculate where a point will be some number of seconds in the future
futurePos :: Point -> Double -> Point
futurePos (x, y) t = (x', y')
    where
      radius = distOrigin (x, y)
      omega = (velocity (x, y)) / radius

      theta = t * omega

      cost = cos theta
      sint = sin theta

      x' = x * cost + y * sint
      y' = y * cost - x * sint

thresh :: Double
thresh = 1000.0

tangentVector :: Point -> Vector
tangentVector (x, y) = normalize (-y, x)

velocity :: Point -> Double
velocity p = let r = distOrigin p in sqrt (mu / r)

computeJump :: Double -> Point -> Point -> Maybe (Vector, Vector)
computeJump time (xs, ys) (xt, yt)
    | time < 2  = error "time was too small!"
    | otherwise = if delta < thresh then Just (dv1, dv2) else Nothing
    where
      radius_sat = distOrigin (xs, ys)
      radius_targ = distOrigin (xt, yt)
      radius_ratio = (-1) * radius_targ / radius_sat
      opp_point = (radius_ratio * xs, radius_ratio * ys)
      targ_point = futurePos (xt, yt) time
      delta = dist opp_point targ_point
      vec = tangentVector (xs, ys)
      dv1 = delta1 radius_sat radius_targ vec
      dv2 = delta1 radius_sat radius_targ vec
