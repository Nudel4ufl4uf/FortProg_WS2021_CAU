import           SimplePrelude

-- A search tree is either empty or is created with an Int as a node with search trees as leafs
data SearchTree = Empty | Node Int SearchTree SearchTree
 deriving Show

-- Some test cases
testTree1 :: SearchTree
testTree1 = Empty

testTree2 :: SearchTree
testTree2 = insert testTree1 5

testTree3 :: SearchTree
testTree3 = insert testTree2 7

testTree4 :: SearchTree
testTree4 = insert testTree3 2

testTree5 :: SearchTree
testTree5 = insert testTree4 3

-- Inserts a number into a search tree so that a new search tree is returned
insert :: SearchTree -> Int -> SearchTree
-- Case: Initial tree is empty -> new one just has th new element as node
insert Empty newelem = Node newelem Empty Empty
-- Inserts the new element into the tree considering the search tree rules
-- Recursive functions ensures that the new element reaches the right spot
insert (Node value left right) newelem | newelem < value = Node value (insert left newelem) right
                                       | otherwise       = Node value left (insert right newelem)

-- Checks if a given Int is part of the search tree
isElem :: SearchTree -> Int -> Bool
-- Case: Initial tree or tree reached by recursion is empty -> can't contain the Int
isElem Empty target = False
-- Recursively going down the tree with the search tree rules
isElem (Node value left right) target | target == value = True
                                      | target < value  = isElem left target
                                      | otherwise       = isElem right target

-- Deletes a given Int out of a search tree and returns the new tree without it
delete :: SearchTree -> Int -> SearchTree
-- Case: Initial tree is empty -> result is also an empty tree
delete Empty del = Empty
-- Start recursive function calls under consideration of the search tree rules
                                   -- Element to delete < value in node -> go on in the left child
delete (Node value left right) del | del < value = Node value (delete left del) right
                                   -- Element to delete > value in node -> go on in the right child
                                   | del > value = Node value left (delete right del)
-- Overall case: del == value (in the node)
-- Case: Tree only has one node that gets deleted -> empty tree
delete (Node value Empty Empty) del = Empty
-- Case: Node only has one child -> return the according side
delete (Node value left Empty) del  = left
delete (Node value Empty right) del = right
-- Case: Node has two children -> replace node with the next higher Int from the right search tree
delete (Node value left right) del  = let
                                       -- Helper function to calculate the next higher Int from the right
                                       minvalue :: SearchTree -> Int
                                       -- No smaller Int on the left anymore -> return the Int of the node...
                                       minvalue (Node value Empty right) = value
                                       -- ...else continue to look for smaller numbers in the left tree
                                       minvalue (Node value left right) = minvalue left
                                       -- The new node we will finally use from the right tree
                                       newnode = minvalue right
                                       -- Recreate the right tree but without the just switched node
                                       right1 = delete right newnode
                                       -- ...used in the final expression to finally return the new search tree
                                       in Node newnode left right1
