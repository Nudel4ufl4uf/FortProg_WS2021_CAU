import           SimplePrelude

-- A polymorph data type for binary trees
data Tree a = Node (Tree a) (Tree a) | Leaf a
 deriving Show

-- Some test trees
testTree1 :: Tree Int
testTree1 = Leaf 3

testTree2 :: Tree Int
testTree2 = Node (Leaf 3) (Leaf 6)

testTree3 :: Tree Int
testTree3 = Node (Node (Leaf 10) (Leaf 5)) (Node (Leaf 6) (Leaf 3))

-- Sums up all values in the leafs of the tree recursively
sumTree1 :: Tree Int -> Int
-- Case: There is just a leaf
sumTree1 (Leaf value)      = value
-- Case: The node has binary trees left and right -> go recursive
-- works because only values from leafs are needed (none in nodes)
sumTree1 (Node left right) = sumTree1 left + sumTree1 right

-- Accumulator method to sum up values in binary tree
sumTree2 :: Tree Int -> Int
-- Calculates with a starting value of 0 and a list of trees
sumTree2 tree = sumTreeAcc 0 [tree]
 where
   -- Case: There isn't anything left (no trees in the list anymore)
   sumTreeAcc acc []                       = acc
   -- Case: Adds value of found leaf to acc und resumes with ts (trees left in the list)
   sumTreeAcc acc ((Leaf value) : ts)      = sumTreeAcc (acc + value) ts
   -- Case: The node has two trees as children so there are no values to add
   -- instead concatenate the children and everything left to sum up and continue
   sumTreeAcc acc ((Node left right) : ts) = sumTreeAcc acc (left : right : ts)

{-
Main> sumTree1 testTree3
24
(0.00 secs, 122,808 bytes)
Main> sumTree2 testTree3
24
(0.00 secs, 123,024 bytes)
-> both methods are equally fast, the accumulator function even needed more bytes
   than the primitive one
-}

--------------------------------------------------------------------------------

-- A polymorph data type for binary trees
data Tree a = Node (Tree a) (Tree a) | Leaf a
 deriving Show

-- Mirrors the binary tree (like on a vertical mirror)
mirrorTree :: Tree a -> Tree a
-- Case: There's only one leaf -> nothing to mirror
mirrorTree (Leaf a)          = Leaf a
-- Otherweise: Swap the children of the node and mirror them recursively
mirrorTree (Node left right) = Node (mirrorTree right) (mirrorTree left)

--------------------------------------------------------------------------------

-- A polymorph data type for binary trees
data Tree a = Node (Tree a) (Tree a) | Leaf a
 deriving Show

-- Some test trees
testTree1 :: Tree Int
testTree1 = Leaf 3

testTree2 :: Tree Int
testTree2 = Node (Leaf 3) (Leaf 6)

testTree3 :: Tree Int
testTree3 = Node (Node (Leaf 10) (Leaf 5)) (Node (Leaf 6) (Leaf 3))

-- Returns all values on the leaves in a list
toList :: Tree a -> [a]
toList (Leaf a)          = [a]
-- Concatenate every value we get through the recursive method calls on the children
toList (Node left right) = toList left ++ toList right

{-
Main> toList testTree1
[3]
(0.00 secs, 123,544 bytes)
Main> toList testTree3
[10,5,6,3]
(0.00 secs, 129,120 bytes)
-> In this case the runtime is equal, but the bytes used indicate that the runtime
   increases wih the amount of nodes in the tree and therefore the height of the tree
-}
