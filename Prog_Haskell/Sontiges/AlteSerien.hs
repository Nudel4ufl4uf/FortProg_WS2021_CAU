import Data.Ratio

--straight forward formula
intSum1:: Int -> Int
intSum1 n = div (n*(n + 1))  2

--with recursion
intSum2:: Int -> Int
intSum2 n
    | n == 0 = 0
    | n /= 0 = n + intSum2(n-1)

--with acc
intSum3:: Int -> Int
intSum3 n = intSum3' n 0
    where
        intSum3' n acc = if n == 0 then acc else intSum3' (n-1) (acc + n)

--calculate the binom of n and k
binom:: Int -> Int -> Int
binom n k = div (fak n 1) (fak k 1 * fak(n-k) 1)
    where fak i acc = if i == 0 then acc else fak (i-1) (acc * i)

--gets binom from the "pascal Dreick"
pascal:: Int -> Int -> Int
pascal row pos
    | row == 0 = 1
    | pos == 0 || pos == row = 1
    | otherwise = pascal (row - 1) (pos - 1) + pascal (row - 1) pos

--create a "Suchbaum"
data SearchTree = Empty | Branch SearchTree Int SearchTree

--insert an element to an existing SearchTree
insert:: Int -> SearchTree -> SearchTree
insert a Empty = Branch Empty a Empty
insert a (Branch tl n tr)
    | a < n = insert a tl
    | a > n = insert a tr
    | otherwise = Branch tl a tr


--check if a given element existst in a given SearchTree
isElem:: Int -> SearchTree -> Bool
isElem a Empty = False
isElem a (Branch tr n tl)
    | a == n = True
    | a < n = isElem a tl
    | a > n = isElem a tr
    |otherwise = False


--delete an specific element from a searchTree
--delete:: Int -> SearchTree -> SearchTree
--delete _ Empty = Empty  
--delete a (Branch tl n tr)
--    | a == n = Branch tl 
--    | a < n = delete a tl  
--    | otherwise = delete a tr

--reverse a list without acc
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

--reverse a list with acc
reverse2:: [a] -> [a]
reverse2 (x:xs) = rev (x:xs) []
    where
        rev []     acc  = xs
        rev (x:xs) acc = rev xs (x : acc)

--return the index of an elemente searched in a list
indexof:: Int -> [Int] -> Maybe Int
indexof i (x:xs) = getindex i (x:xs) 0
    where
        getindex i [] acc     = Nothing
        getindex i (x:xs) acc = if i == x then Just acc else getindex i xs (acc+1)

--add an elemt to a list somewhere?
add:: a -> [[a]] -> [[a]]
add x = map (x :)

 --get the head elements of a list?
inits:: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : add x (inits xs)

--get the tail elements of a list?
tails:: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

--insert an elemente at avery position of agiven list
insert2:: a -> [a] -> [[a]]
insert2 e []  = [[e]]
insert2 e (x:xs) = res e (x:xs) [e]
    where
        res e [] acc = [acc]
        res e (x:xs) acc = (acc ++ (x:xs)) : res e xs (x : acc)

--permutation of a list of elements
perm:: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = ret [] (perm xs)
    where
        ret acc []       = acc
        ret acc (ys:yss) = ret (acc ++ insert2 x ys) yss

--data type JSON
data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show

--folding function for the data type JSON
foldJson:: a -> (Bool -> a) -> (Int -> a) -> (Float -> a) -> (String -> a) -> ([a] -> a) -> ([(String, a)] -> a) ->  JSON  -> a
foldJson jnull jbool jint jfloat jstring jarray jobject doc = case doc of
    JNull     -> jnull
    JBool b   -> jbool b
    JInt i    -> jint i
    JFloat f  -> jfloat f
    JString s -> jstring s
    JArray a  -> jarray (map (foldJson jnull jbool jint jfloat jstring jarray jobject)a)
    JObject o -> jobject (map foldKV o)
        where
            foldKV (k, v) = (k, foldJson jnull jbool jint jfloat jstring jarray jobject v)

sumList:: [Int] -> Int
sumList = sum

--data binary tree
data BinTree a = Node a (BinTree a) (BinTree a) | Leaf
    deriving (Show, Eq)

--folding dunction for a tree
foldTree:: (a -> b -> b -> b) -> b -> BinTree a -> b
foldTree node leaf = f
    where
        f (Node x left right)    = node x (f left) (f right)
        f Leaf                = leaf

--an example bintree
bintree = Node 1 (Node 2 (Node 3 Leaf Leaf) (Node 3 Leaf Leaf) ) (Node 2 (Node 3 Leaf Leaf) (Node 3 Leaf Leaf))

--sum function for a given bin tree using folding     
sumBinTree :: Num a => BinTree a -> a
sumBinTree = foldTree (\val res_left res_right -> val + res_left + res_right) 0

--data type for a normal tree structure
data Tree a = (Tree a) :&: (Tree a) | Leeaf a
    deriving (Show, Eq)

--function to map over a tree
--mapTree :: (a -> b) -> Tree a -> Tree b 
--mapTree f leaf = leaf
--mapTree f (Node l r) = Node (f l) (f r)

--an example tree
--tree1 = Tree (Leeaf 2) :&: Tree (Leeaf 10) 

--map function via folding
mapf :: (a -> b ) -> [a] -> [b]
mapf = map

--unzip function vio folding
unzipf:: [(Int,Int)] -> ([Int], [Int])
unzipf = foldr (\(a,b) ~ (as,bs) -> (a:as, b:bs)) ([],[])

--nub function via folding
nubf :: [Int] -> [Int]
nubf = foldr (\x xs -> x : filter (x/=) xs) []

--data strucutre for Rose Trees 
data Rose a = Rose a [Rose a]

--EQ instance for RoseTrees
instance Eq a => Eq (Rose a) where
    Rose x ts == Rose y us = x == y && ts == us

--Ord instance for RoseTrees
instance Ord a => Ord (Rose a) where
       compare (Rose x ts) (Rose y us) = case compare x y of
           EQ -> compare ts us
           cp -> cp

--pretty class
class Pretty a where
    pretty:: a -> String

--pretty instance for RoseTrees
instance Pretty a => Pretty (Rose a) where
    pretty t = prettyTree 0 t
        where
            prettyTree n (Rose x ts) = intercalate "\n"
                             $ prettyIndented n x : map (prettyTree (n + 1)) ts
            prettyIndented 0 x = pretty x
            prettyIndented n x = concat (replicate (n - 1) "|   ") ++ "+-- " ++ pretty x
--helper function for pretty instance
intercalate :: [a] -> [[a]] -> [a]
intercalate _   []     = []
intercalate _   [x]    = x
intercalate sep (x:xs) = concat [x, sep, intercalate sep xs]

--type of "Menge" 
type IntSet = Int -> Bool

--example IntSet
lostSet :: IntSet
lostSet x = x `elem` [4, 8, 15, 16, 23, 42]

--empty "Menge"
empty:: IntSet
empty x = x `elem` []
--or: empty = \_ -> False 

--remove an Element from the IntSet
--remove :: Int -> IntSet -> IntSet
--remove x m = difference m (x ==)

--inert an element into the IntSet
insertT:: Int -> IntSet -> IntSet
insertT x = union (x ==)
--insertT = union . (==)

--check if the IntSet contains the given Element
isElemT :: Int -> IntSet -> Bool
isElemT x s = s x

--union two "Mengen"
union :: IntSet -> IntSet -> IntSet
union s1 s2 x = s1 x || s2 x

--create the deiffrerence of two "Mengen"
--difference:: IntSet -> IntSet -> IntSet
--difference s1 s2 = \x -> x == xor (isElemT x s1 isElemT x s2) 

--create an infinite list of Fib numbers 
fib:: [Integer]
fib = fibsgen 0 1
  where
    --generate the infinite list of fib numbers when asked to do so
    fibsgen:: Integer -> Integer -> [Integer]
    fibsgen n1 n2 = n1 : fibsgen n2 (n1 + n2)

goldenRation:: [Rational]
goldenRation = zipWith (\fibn fibnp1 -> 1 + fibn % fibnp1) fib (tail fib)


approx:: Rational -> [Rational] -> Rational
approx eps (x:y:ys)
    | abs (x - y) <= eps = y
    | otherwise          = approx eps (y:ys)
approx _ _               = error "approx: finite list"

allCombinations :: [a] -> [[a]]
allCombinations [] = [[]]
allCombinations v = [] : concatMap (\w -> map (:w) v) (allCombinations v)

data Child = Naughty | Nice

data Wish a = Wish a

data Present a = Box a | WrappingPaper (Present a)

instance Eq Child where
  x == y
    | x == Naughty = y == Naughty
    | y == Naughty = False
    | otherwise = True

wrap :: Int -> a -> Present a
wrap 0 x = Box x
wrap n x = WrappingPaper (wrap(n - 1) x)

santa :: [(Child, Wish a)] -> [(Child, Maybe (Present a))]
santa xs = snd (foldr santasLittleHelper (0, []) xs)
  where santasLittleHelper = \(c, Wish x) (n,cs) ->
          let c' = if c == Naughty then (c, Nothing) else (c, Just (wrap n x))
          in ( n + 1,c':cs)

data Treet a = Leaft a | Nodet (Treet a) (Treet a)
    deriving Show


instance Functor Tree where
    fmap = mapTree

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = error "not implemented"

prod :: Num a => [a] -> a
prod xs = foldr (+) 1 xs

--boomBang :: Integral a => Ord a => Num a => [a] -> [b]
--boomBang xs = [if x < 10 then "Boom!" else "Bang" | x <- xs, odd x] 

triangels :: Integral a => Ord a => [(a,a,a)]
triangels = [(a,b,c) | c <- [1 .. 10], b <- [1 ..10], a <- [1 ..10], a^2 + b^2 == c^2]

--reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++  [x]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)
        biggersorted  = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggersorted  

compareWH :: (Num a, Ord a) => a -> Ordering 
compareWH = compare 100

data NewTree a = NewLeaf a | NewNode (NewTree a) (NewTree a)
    deriving Show 

------------------------------------------------------------------------------------
--Functors

instance Functor NewTree where 
    fmap f (NewLeaf a) = NewLeaf (f a)
    fmap f (NewNode l r) = NewNode (fmap f l) (fmap f r)

instance Functor Maybe where 
    fmap f Nothing = Nothing 
    fmap f (Just a) = Just (f a)

-------------------------------------------------------------------------------------
--Applicatives

instance Applicative NewTree where
    pure a = NewLeaf a
    (NewLeaf f) <*> t = fmap f t
    (NewNode l r) <*> t = NewNode (l <*> t) (r <*> t)

instance Applicative Maybe where
    pure a = Just a
    Nothing <*> _ = Nothing 
    Just a <*> something = Just a something
-------------------------------------------------------------------------------------
--Monads

instance Monad NewTree where
    return a = NewLeaf a
    (NewLeaf a) >>= f = f a
    (NewNode l r) >>= f = NewNode (l >>= f) (r >>= f) 

instance Monad Maybe where
    return a = Just a
    Nothing >>= f = Nothing 
    (Just a) >>= f = Just (f a)

data Set a = Set [a]
  deriving Show

instance Arbitrary a => Arbitrary (Set a) where
  arbitrary = do
    xs <- arbitrary
    return (Set xs)

empty :: Set a
empty = Set []

isEmpty :: Set a -> Bool
isEmpty (Set xs) = null xs

insert :: a -> Set a -> Set a
insert x (Set xs) = Set (x:xs)

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = elem x xs

delete :: Eq a => a -> Set a -> Set a
delete x (Set xs) = Set (remove x xs)
  where
  remove _ []                 = []
  remove y (z:zs) | y == z    = zs
                  | otherwise = remove y zs

union :: Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (xs ++ ys)

intersect :: Ord a => Set a -> Set a -> Set a
intersect (Set s1) (Set s2) = Set (merge s1 s2)
  where
  merge []     ys     = ys
  merge xs     []     = xs
  merge (x:xs) (y:ys) = case compare x y of
    LT -> x : y : merge xs ys
    EQ -> y : merge xs ys
    GT -> y : x : merge xs ys

size :: Set a -> Int
size (Set xs) = length xs  

prop_isEmpty_empty :: Bool
prop_isEmpty_empty = isEmpty empty

prop_member_empty :: Int -> Bool
prop_member_empty x = not (member x empty)

prop_size_empty :: Bool
prop_size_empty = size empty == 0

prop_isEmpty_insert :: Int -> Set Int -> Bool
prop_isEmpty_insert x s = not (isEmpty (insert x s))

prop_member_insert :: Int -> Set Int -> Bool
prop_member_insert x s = member x (insert x s)

prop_member_insert2 :: Int -> Int -> Set Int -> Property
prop_member_insert2 x y s = x /= y ==> member y (insert x s) == member y s

prop_size_insert :: Int -> Set Int -> Bool
prop_size_insert x s =
  size (insert x s) == size s + (if member x s then 0 else 1)

prop_isEmpty_delete :: Int -> Set Int -> Property
prop_isEmpty_delete x s = isEmpty s ==> isEmpty (delete x s)

prop_member_delete :: Int -> Set Int -> Bool
prop_member_delete x s = not (member x (delete x s))

prop_member_delete2 :: Int -> Int -> Set Int -> Property
prop_member_delete2 x y s = x /= y ==> member y (delete x s) == member y s

prop_size_delete :: Int -> Set Int -> Bool
prop_size_delete x s =
  size (delete x s) == size s - (if member x s then 1 else 0)

prop_isEmpty_union :: Set Int -> Set Int -> Bool
prop_isEmpty_union s1 s2 = isEmpty (union s1 s2) == (isEmpty s1 && isEmpty s2)

prop_member_union :: Int -> Set Int -> Set Int -> Bool
prop_member_union x s1 s2 =
  (member x s1 || member x s2) == member x (union s1 s2)

prop_size_union :: Set Int -> Set Int -> Bool
prop_size_union s1 s2 =
  size (union s1 s2) == size s1 + size s2 - size (intersect s1 s2)

prop_isEmpty_intersect :: Set Int -> Set Int -> Property
prop_isEmpty_intersect s1 s2 =
  isEmpty s1 || isEmpty s2 ==> isEmpty (intersect s1 s2)

prop_member_intersect :: Int -> Set Int -> Set Int -> Bool
prop_member_intersect x s1 s2 =
  (member x s1 && member x s2) == member x (intersect s1 s2)

prop_size_intersect :: Set Int -> Set Int -> Bool
prop_size_intersect s1 s2 = s >= 0 && s <= size s1 && s <= size s2
  where s = size (intersect s1 s2)