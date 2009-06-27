module TeamCA.Strategies.Util where

-- |A point, using standard Cartesian coordinates; the tuple is of the form
-- (x, y).
type Point = (Double, Double)

type Velocity = Double

-- |Newton's gravitaional constant
gConst :: Double
gConst = 6.67428e-11

-- |The mass of the Earth, in kilograms
earthMass :: Double
earthMass = 6.0e24

-- |The radius of the Earth, in meters
earthRadius :: Double
earthRadius = 6.357e6

-- |Get the delta v to transfer from an elliptical to circular orbit. C.f. pp
-- 400 Kleppner & Kolenkow. Example: ecDeltaV 7900 = 586.02, so if a satellite
-- is travelling at 7900 m/s at perigee and wants to transfer to a circular
-- orbit, it should reduce its velocity by 586 m/s.
ecDeltaV :: Velocity -> Velocity
ecDeltaV v = (1 - sqrt (6 / 7)) * v

distance :: Point -> Point -> Double
distance (sx1, sy1) (sx2, sy2) = sqrt ((sx1 - sx2)**2 + (sy1 - sy2)**2)

-- |Get the radius of the satellite.
radiusOf :: Point -> Double
radiusOf p = distance (0, 0) p


