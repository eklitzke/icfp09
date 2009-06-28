module TeamCA.Math where

distOrigin = dist (0, 0)

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x0, y0) (x1, y1) = sqrt $ (x1 - x0) ^ 2 + (y1 - y0) ^ 2

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


