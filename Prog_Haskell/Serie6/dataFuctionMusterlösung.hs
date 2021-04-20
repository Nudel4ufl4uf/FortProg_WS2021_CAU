-- Type for number sets represented by an indicator function
type IntSet = Int -> Bool

lostSet :: IntSet
lostSet = \x -> elem x [4,8,15,16,23,42]

-- Empty number set
empty :: IntSet
empty = \_ -> False

-- Insert number into number set
insert :: Int -> IntSet -> IntSet
insert x s = \y -> if x == y then True else s y

insert' :: Int -> IntSet -> IntSet
insert' x s = union (\y -> x == y) s

insert'' :: Int -> IntSet -> IntSet
insert'' = union . (==)
insert'' x s = union (x ==) s     -- Left section as anonymous function
insert'' x s = union ((==) x) s   -- Prefix notation for (==)
insert'' x s = (union . (==)) x s -- Rearrange

-- Remove a number from a number set
remove :: Int -> IntSet -> IntSet
remove x m = difference m (x ==)

remove' :: Int -> IntSet -> IntSet
-- remove' x m = (flip difference) (x ==) m
remove' = flip difference . (==)

-- Test for a number in a number set
isElem :: Int -> IntSet -> Bool
isElem x s = s x

isElem' :: Int -> IntSet -> Bool
-- isElem x s = s $ x
-- isElem x s = ($) s x
-- isElem x s = flip ($) x s
-- isElem = flip ($)
isElem' = flip id

-- Build the union of two number sets
union :: IntSet -> IntSet -> IntSet
union s1 s2 = \x -> s1 x || s2 x

-- Build the intersection of two number sets
intersection :: IntSet -> IntSet -> IntSet
intersection s1 s2 = \x -> s1 x && s2 x

-- Build the complement of a number set
complement :: IntSet -> IntSet
complement m = \x -> not (m x)

complement' :: IntSet -> IntSet
-- complement m x = not (m x)
-- complement m = not . m
complement' = (not .)

-- Build the difference of two number sets
difference :: IntSet -> IntSet -> IntSet
difference m n = intersection m (complement n)
-- difference m = intersection m . complement

--------------------------------------------------------------------------------

--Aus einer Liste konstruieren wir eine Menge, indem wir für eine Zahl schauen ob
--sie in der Liste enthalten ist:
-- Convert list of numbers to number set
listToSet :: [Int] -> IntSet
listToSet xs = \x -> x `elem` xs
-- listToSet xs x = x `elem` xs
-- listToSet = flip elem
