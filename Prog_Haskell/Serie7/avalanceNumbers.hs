import           Prelude

-- n is an avalanche number when there are i, j and k in that fashion
-- that n = 3^i * 5^j * 7^k
-- defines an infinte list of avalanche numbers
avalanche :: [Integer]
avalanche = filter avaCheck [1, 3 ..]
 where
  -- checks whether a number is eligable
  avaCheck :: Integer -> Bool
  -- x is a "base number" of the avalanche condition
  avaCheck x | x == 1 || x == 3 || x == 5 || x == 7 = True
             -- works as following:
             -- case x is avalanche number: i, j and k are whole numbers
             -- -> the respective exponent is decreased by one and we will
             --    finally end up at a "base number"
             -- case x isn't avalanche number: by dividing it could
             -- eventually happens that i, j and/or k couldn't be written
             -- as whole numbers
             -- -> we will not end up at a "base number" but in the
             --    otherwise case
             | mod x 3 == 0               = avaCheck (div x 3)
             | mod x 5 == 0               = avaCheck (div x 5)
             | mod x 7 == 0               = avaCheck (div x 7)
             | otherwise                  = False
