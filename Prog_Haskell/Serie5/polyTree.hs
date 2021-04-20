import           SimplePrelude

-- A polymorph data type for binary trees
data Tree a = Node (Tree a) (Tree a) | Leaf a
 deriving Show

-- Some test trees
testTree :: Tree (Tree Int)
testTree = Node (Leaf (Node (Leaf 2) (Leaf 3))) (Leaf (Leaf 2))

testTree2 :: Tree Int
testTree2 = flatTree testTree

-- Flattens a tree of of trees to a tree of elements
flatTree :: Tree (Tree a) -> Tree a
-- Element x in the leaf is a tree itself -> just return it
flatTree (Leaf x)          = x
-- Flatten the side trees until a leaf is reached
flatTree (Node left right) = Node (flatTree left) (flatTree right)

-- Applies a function f to every element in the tree
mapTree :: (a -> b) -> Tree a -> Tree b
-- Case: Just one element -> apply function to it
mapTree f (Leaf x)          = Leaf (f x)
-- Apply the function recursively to the side trees
mapTree f (Node left right) = Node (mapTree f left) (mapTree f right)

-- Folds together a tree to a single value
--             Funktion für node (type b because the function for
--  Funktion für leaf  returns an element of type b)
foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree leaf node tree = case tree of
  -- To apply the function for leaves to x
  Leaf x          -> leaf x
  -- Recursively apply the node function with giving the function onto the side trees
  Node left right -> node (foldTree leaf node left) (foldTree leaf node right)

-- Extends a tree with other trees on the leaves
--      Funktion für Leaf und Node
extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f (Leaf a)          = f a
extendTree f (Node left right) = Node (extendTree f left) (extendTree f right)
