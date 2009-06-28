module TeamCA.Math.Ellipse
    ( Position
    , Angle
    , Orientation
    , Ellipse(Ellipse)
    , isCircle
    , distance
    , reorient
    , paramEllipse ) where


type Position = (Double, Double)

type Angle = Double
type Orientation = Angle

data Ellipse = Ellipse { orientation :: Orientation
                       , a :: Double
                       , e :: Double
                       } deriving (Show, Eq)

tolerance :: Double
tolerance = 0.01

-- Check whether or not an ellipse is a circle
isCircle :: Ellipse -> Bool
isCircle ellipse = e ellipse < tolerance

-- Given a position, get the angle with respect to the origin. The angle will be
-- in the range [0, 2*pi]
pointToAngle :: Position -> Angle
pointToAngle (x, 0)
    | x > 0     = 0
    | otherwise = pi
pointToAngle (0, y)
    | y > 0     = pi / 2
    | otherwise = 3 * pi / 2
pointToAngle (x, y)
    | (x > 0) && (y > 0) = theta
    | (x < 0) && (y < 0) = pi + theta
    | (x < 0) && (y > 0) = pi + theta
    | (x < 0) && (y < 0) = 2*pi + theta
    where
      theta = atan (y / x)

distance :: Position -> Position -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)**2 + (y2 - y1)**2)

radiusOf :: Position -> Double
radiusOf = distance (0, 0)

-- |Given two points, this returns the pair (a, e). This assumes that the points
-- are given in the relative coordinate system where the x-axis extends out to
-- the right along the major axis of the ellipse (with a coordinate like (x, 0)
-- passing through the apogee, with x positive); hence the moniker "naive".
paramEllipseNaive :: Position -> Position -> (Double, Double)
paramEllipseNaive p1 p2 = (a, e)
    where
      phi = cos . pointToAngle
      (r1, r2) = (radiusOf p1, radiusOf p2)
      (phi1, phi2) = (phi p1, phi p2)
      e = (r1 + r2) / (phi1 * r1 + phi2 * r2)
      a = (e * phi1 - 1) * r1 / (e**2 - 1)

-- This reorients a point in a new frame of reference
reorient :: Orientation -> Position -> Position
reorient theta (x, y) = (x', y')
    where
      c = cos theta
      s = sin theta
      x' = x*c + y*s
      y' = y*c - x*s

paramEllipse :: Orientation -> Position -> Position -> Ellipse
paramEllipse theta p1 p2 = Ellipse theta a e
    where
      p1' = reorient theta p1
      p2' = reorient theta p2
      (a, e) = paramEllipseNaive p1' p2'
