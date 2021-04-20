import           SimplePrelude

-- computes the binom of n and k
binom :: Int -> Int -> Int
binom n k = if k == 0 then 1
                      else if n == 0 then 0
                      else div (fac n) (fac k * fac (n-k))

-- computes the factorial of m
fac :: Int -> Int
fac m = if m == 0 then 1
                  else m * fac (m-1)
