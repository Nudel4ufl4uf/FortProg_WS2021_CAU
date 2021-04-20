import           SimplePrelude

fac :: Int -> Int
fac n = if n == 0 then 1 else n * fac (n - 1)

binom :: Int -> Int -> Int
binom n k = div (fac n) (fac k * fac (n - k))
