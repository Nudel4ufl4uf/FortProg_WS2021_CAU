
--trying a infix version of minus or whaterver function with a `minus` b
minus:: Int -> Int -> Int
minus a b = a - b

--try pattern matching simple
isZero:: Int -> Bool
iszero 0 = True
isZero _ = False

--tail recursive function of faculty
fac:: Int -> Int
fac n = aux n 1
    where
      aux n acc
        | n <= 1      = acc
        | otherwise   = aux (n-1) (n*acc)

--generate a constante list
asc:: Int -> Int -> [Int]
asc n m
  | m < n   = []
  | m == n  = [m]
  | m > n   = n: asc (n+1) m

-- try out list pater matching
-- sum:: [Int] -> Int
-- sum []     = 0
-- sum (x:xs) = x + sum xs

--try out list pattern matching
evens:: [Int] -> [Int]
evens []  = []
evens (x:xs)
  | mod x 2 == 0 = x :evens xs
  | otherwise    = evens xs

--add components of tuples and put them into a list
addTuples:: [(Int,Int)] -> [Int]
addTuples xs = [x + y | (x,y) <- xs]

-- function that checks if  agiven Element is in the given list
-- elem:: Int -> [Int] -> Bool--
-- elem _ []     = False
-- elem e (x:xs) =  (e == x) || (elem e xs)

--function that deletes all duplicates of a list
nub2:: [Int] -> [Int]
nub2 [] = []
nub2 (x:xs)
  | elem x xs    = nub2 xs
  | otherwise    = x : nub2 xs

--check if a given list has an ascending order
isAsc:: [Int] -> Bool
isAsc []       = True
isAsc [x]      = True
isAsc (x:y:xs)=(x <= y) && isAsc (y:xs)

--check if two nodes in a directed Grph have a connection path
hasPath:: [(Int,Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs x y
  | x == y  = True
  | otherwise = let
    xs' = [(n,m) | (n,m) <- xs, n /= x]
    in or [hasPath xs' m y | (n,m) <- xs, n == x]

--simple example for higher order functions
app:: (a -> b) -> a -> b
app f x = f x

add1:: Int -> Int
add1 x = x + 1

-- -> do: app add1 1 => 2

-- importand higher order funcions

-- map:: (a -> b) -> [a] -> [b]
-- example: map (\x -> x*x) [1,2,3,4,5] result: [1,4,9,16,25]
doubleList:: (Num a) => [a] -> [a]
doubleList [] = []
doubleList xs = map (\x -> 2*x) xs

-- filtering elements in an array according to a certain condition
-- filter:: (a -> Bool) -> [a] -> [a]
-- example: filter (\x -> x>2) [1,2,3,4,5] result: [3,4,5]

 --function composition examlpes
 -- map2D :: (a -> b) -> [[a]] -> [[b]]
 -- map2D = map . map
 -- map2D f xs = map (\ys -> map f ys) xs

-- Exersize on folding
-- function to reverse an array
rev:: [a] -> [a]
rev (x:xs) = foldl(\acc x -> x:acc) [] (x:xs)

-- function
prefixes:: [a] -> [[a]]
prefixes (x:xs) = foldr (\x acc -> [x]: (map((:)x)acc)) [] (x:xs)
