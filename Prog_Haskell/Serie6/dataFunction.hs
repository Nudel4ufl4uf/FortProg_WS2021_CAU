import           Prelude

-- IntSet is a function that takes an Int and returns a bool
-- True if Int is element of the IntSet
type IntSet = Int -> Bool

-- two test sets
lostSet :: IntSet
lostSet = \x -> elem x [4, 8, 15, 16, 23, 42]

testSet :: IntSet
testSet = \x -> elem x [1, 5, 8, 14, 16, 25, 42]

-- represents the empty set
empty :: IntSet
empty = \x -> elem x []

-- inserts a number into a set
insert :: Int -> IntSet -> IntSet
-- check whether x is element of set with new y: either x is the
-- inserted element or x is in the initial set => True, else => False
insert y set = \x -> x == y || set x

-- removes a number from a set
remove :: Int -> IntSet -> IntSet
-- check whether x is element of set without y: x is not the removed
-- number and is in the initial set => True, else => False
remove y set = \x -> x /= y && set x

-- checks if a number is element of a set
isElem :: Int -> IntSet -> Bool
-- using the definition of our IntSet we can just check is y is in set
isElem y set = set y

-- unites two sets
union :: IntSet -> IntSet -> IntSet
-- checks whether x is element of the union of set1 and set2: x has
-- to be in set1 or set2 => True, else => False
union set1 set2 = \x -> set1 x || set2 x

-- creates the intersection of two sets
intersection :: IntSet -> IntSet -> IntSet
-- checks whether x is element of the intersection of set1 and set2:
-- x hast to be in both sets => True, else => False
intersection set1 set2 = \x -> set1 x && set2 x

-- creates the difference between two sets
difference :: IntSet -> IntSet -> IntSet
-- checks whether x is element of the difference of set1 and set2:
-- x is in set1 but not in set2 => True, else => False
difference set1 set2 = \x -> set1 x && not(set2 x)

-- creates the complement of a set (all numbers not in the set)
complement :: IntSet -> IntSet
-- checks whether x is element of the complement of set: x mustn't
-- be in set => True, else (x is in set) => False
complement set = \x -> not(set x)


--------------------------------------------------------------------------------


-- IntSet is a function that takes an Int and returns a bool
-- True if Int is element of the IntSet
type IntSet = Int -> Bool

-- some test cases
lostSet :: IntSet
lostSet = \x -> elem x [4, 8, 15, 16, 23, 42]

testSet :: IntSet
testSet = \x -> elem x [1, 5, 8, 14, 16, 25, 42]

testList :: [Int]
testList = [1, 2, 3, 4, 5]

-- represents the empty set
empty :: IntSet
empty = \x -> elem x []

-- inserts a number into a set
insert :: Int -> IntSet -> IntSet
-- check whether x is element of set with new y: either x is the
-- inserted element or x is in the initial set => True, else => False
insert y set = \x -> x == y || set x

-- unites two sets
union :: IntSet -> IntSet -> IntSet
-- checks whether x is element of the union of set1 and set2: x has
-- to be in set1 or set2 => True, else => False
union set1 set2 = \x -> set1 x || set2 x

-- converts a list to a set
listToSet :: [Int] -> IntSet
-- unite the set of x inserted to the empty set with the rest recursively
listToSet (x : xs) = union (insert x empty) (listToSet xs)

{-
setToList :: IntSet -> [Int] can't be defined because the only way
we can access the numbers in the set is through a boolean value.
-> We can't iterate across the set to insert the values into a list.
-}
