sumL:: (Num a) => [a] -> a
sumL xs = foldl (+) 0 xs

sumR:: (Num a) => [a] -> a
sumR xs = foldr (+) 0 xs

sumL2:: Num a => [a] -> a
sumL2 xs = foldl1 (+) xs

sumR2:: Num a => [a] -> a
sumR2 xs = foldr1 (+) xs

product' :: (Num a) => [a] -> a
product' xs = foldr1 (*) xs

data FC = FC {name :: String, year :: Int, league :: String, titles :: Int, jearsyColor :: String }
