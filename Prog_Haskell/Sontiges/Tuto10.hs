import Test.QuickCheck

data Set a = Set [a]
  deriving Show

instance Arbitrary a => Arbitrary (Set a) where
  arbitrary = do
    xs <- arbitrary
    return (Set xs)

empty :: Set a
empty = Set []

isEmpty :: Set a -> Bool
isEmpty (Set [x]) = False
isEmpty _         = True

insert :: a -> Set a -> Set a
insert x (Set xs) = Set (x:xs)

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = elem x xs

union :: Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (xs ++ ys)

prop_isEmpty_empty :: Bool
prop_isEmpty_empty = isEmpty empty

prop_member_empty :: Int -> Bool
prop_member_empty x = not (member x empty)

prop_isEmpty_insert :: Int -> Set Int -> Bool
prop_isEmpty_insert x s = not (isEmpty (insert x s))

prop_member_insert :: Int -> Set Int -> Bool
prop_member_insert x s = member x (insert x s)

prop_member_insert2 :: Int -> Int -> Set Int -> Property
prop_member_insert2 x y s = x /= y ==> member y (insert x s) == member y s

prop_isEmpty_union :: Set Int -> Set Int -> Bool
prop_isEmpty_union s1 s2 = isEmpty (union s1 s2) == (isEmpty s1 && isEmpty s2)

prop_member_union :: Int -> Set Int -> Set Int -> Bool
prop_member_union x s1 s2 =
  (member x s1 || member x s2) == member x (union s1 s2)

main :: IO ()
main = do
  putStr "prop_isEmpty_empty    : "
  quickCheck prop_isEmpty_empty
  putStr "prop_member_empty     : "
  quickCheck prop_member_empty
  putStr "prop_isEmpty_insert   : "
  quickCheck prop_isEmpty_insert
  putStr "prop_member_insert    : "
  quickCheck prop_member_insert
  putStr "prop_member_insert2   : "
  quickCheck prop_member_insert2
  putStr "prop_isEmpty_union    : "
  quickCheck prop_isEmpty_union
  putStr "prop_member_union     : "
  quickCheck prop_member_union