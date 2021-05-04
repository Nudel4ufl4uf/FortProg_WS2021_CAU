import SimplePrelude hiding (length, (++), last, head, tail, concat, fst, snd, zip, unzip)

-- Compute the length of a list of integers:
lengthInt :: [Int] -> Int
lengthInt []     = 0
lengthInt (x:xs) = 1 + lengthInt xs

-- Compute the length of a list of floats:
lengthFloat :: [Float] -> Int
lengthFloat []     = 0
lengthFloat (x:xs) = 1 + lengthFloat xs

-- Compute the length of a list of characters:
lengthChar :: [Char] -> Int
lengthChar []     = 0
lengthChar (x:xs) = 1 + lengthChar xs

-- Compute the length of a list of elements:
length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

append :: [a] -> [a] -> [a]
append []       ys = ys
append (x : xs) ys = x : (append xs ys)

(++) :: [a] -> [a] -> [a]
[]       ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

-- Compute the last element of a list:
last :: [a] -> a
last [x]    = x
last (x:xs) = last xs

data List a = Nil | Cons a (List a)
 deriving Show -- automatically define a "show" function for IntList
-- Haskell: data [a] = [] | a : [a]

---------------------------------------------------------------------

-- The type of possible values: maybe there is some value of type 'a':
--data Maybe a = Nothing | Just a

noValue :: Maybe a
noValue = Nothing

aOne :: Maybe Int
aOne = Just 1

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just x) = False

maybeLast :: [a] -> Maybe a
maybeLast []     = Nothing
maybeLast [x]    = Just x
maybeLast (x:xs) = maybeLast xs

---------------------------------------------------------------------
-- Binary trees:
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

tree123 :: Tree Int
tree123 = Node (Leaf 1)
               (Node (Leaf 2)
                     (Leaf 3))

treeABC :: Tree Char
treeABC = Node (Leaf 'a')
               (Node (Leaf 'b')
                     (Leaf 'c'))

-- The height of a tree:
height :: Tree a -> Int
height (Leaf x)     = 1
height (Node t1 t2) = 1 + max (height t1) (height t2)

---------------------------------------------------------------------

-- Further functions on lists:

-- The first list element:
head :: [a] -> a
head (x:xs) = x

-- better:
maybeHead :: [a] -> Maybe a
maybeHead []     = Nothing
maybeHead (x:xs) = Just x

-- The rest of a non-empty list:
tail :: [a] -> [a]
tail (x:xs) = xs

-- Computes the n-th element of a non-empty list (n=0: head of the list)
nth :: [a] -> Int -> a
nth (x:xs) n = if n == 0 then x
                         else nth xs (n - 1)
-- > predefined as (!!) in Haskell

-- Concatenates a list of list of elements to a single list of elements
concat :: [ [a] ] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

hello :: String
--hello = ['H','e','l','l','o']
hello = "Hello"

-- Union types:
data Union a b = This a | That b
--data Either a b = Left a | Right b

anIntStringList :: [Either Int String]
anIntStringList = [Left 1, Right "Hello", Left 2, Right "World"]

sumInts :: [Either Int a] -> Int
sumInts []             = 0
sumInts (Left i  : xs) = i + sumInts xs
sumInts (Right _ : xs) = sumInts xs

data Pair a b = Pair a b
data Triple a b c = Triple a b c

aTriple :: Triple Int String Bool
aTriple = Triple 1 "Hello" True

hTriple :: (Int, String, Bool)
hTriple = (1, "Hello", True)

fst :: (a,b) -> a
fst (x,y) = x

snd :: (a,b) -> b
snd (x,y) = y

-- Combine pairwise the elements of two lists to one:
zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


-- Split a list of pairs into the lists of the components:
unzip :: [(a,b)] -> ([a], [b])
unzip []            = ([], [])
unzip ((x,y) : xys) = let (xs,ys) = unzip xys
                      in (x:xs, y:ys)
