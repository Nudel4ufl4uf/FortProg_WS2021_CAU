import           Data.Ratio

--infinite list of all Fibonacci numbers
fibs:: [Integer]
fibs = fibsgen 0 1
  where
    --generate the infinite list of fib numbers when asked to do so
    fibsgen:: Integer -> Integer -> [Integer]
    fibsgen n1 n2 = n1 : fibsgen n2 (n1 + n2)

--return the n-th fib numbers
fib:: Int -> Integer
fib n = fibs !! (n - 1)

--List of all approximations on the goldenRatio
goldenRatio:: [Rational]
--start with the first fib number
goldenRatio = gold 1
  where
    --create a infinite List of goldenRatios when asked to do so
    gold:: Int -> [Rational]
    gold n = 1 + (fib n) % (fib (n + 1)) : gold (n+1)

--Find the first approximation from the goldenRatio list
--wich exceeds the asked eps value.
approx :: Rational -> [Rational] -> Rational
--check on an empty list wich cannot be checked
approx eps []       = 0
--find the first approximation by calculation the distance between n (y) and n-1 (x)
approx eps (x:y:xs) = if abs (x - y) <= eps then y
                    --else go through the list of approximations rekursively
                    else approx eps (y:xs)
