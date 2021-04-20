import           Data.Ratio

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

goldenRatio :: [Rational]
goldenRatio =
  zipWith (\fibn fibnp1 -> 1 + (fibn % fibnp1)) fibonacci (tail fibonacci)

approx :: Rational -> [Rational] -> Rational
approx eps (x:y:ys)
  | abs (x - y) <= eps = y
  | otherwise          = approx eps (y:ys)
approx _   _           = error "approx: finite list"
