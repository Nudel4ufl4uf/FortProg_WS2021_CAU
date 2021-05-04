import Prelude

--Präsenzaufgabe 7:
--------------------------------------------------------------------------------
data BTree a = Leaf a
             | Node (BTree a) a (BTree a)
  deriving Show

foldBTree :: (a -> b) -> (b -> a -> b -> b) -> BTree a -> b
foldBTree fleaf fnode bt = case bt of
  Leaf x     -> fleaf x
  Node l n r -> fnode (foldBTree fleaf fnode l) n (foldBTree fleaf fnode r)

maxBTreeWithoutFold:: BTree Int -> Int
maxBTreeWithoutFold (Leaf x)     = x
maxBTreeWithoutFold (Node l n r) = max (max (maxBTreeWithoutFold l) n) (maxBTreeWithoutFold r)

maxBTree:: BTree Int -> Int
maxBtree bt = foldBTree (\x -> x) (\maxl x maxr -> max ( max maxl x) maxr) bt

replace:: BTree a -> b -> BTree b
replace bt y = foldTree (\_-> Leaf y) (\ly x ry) bt

replaceWithoutFold:: BTree a -> b -> BTRee b
replaceWithoutFold (Leaf _)     y = Leaf y
replaceWithoutFold (Node l _ r) y = Node (replaceWithoutFold l y) y (replaceWithoutFold r y)

replaceMaxRecNaive:: BTree Int -> a -> (BTree a, Int)
replaceMaxRecNaive bt y = (replace bt y, maxBTree bt)

replcaMaxRec:: BTree Int -> a ->(BTree a, Int)
replaceMaxRec (Leaf x)     y = (Leaf y, x)
replaceMaxRec (Node l x r) y = (Node y,..)
  where
    (ly, maxl) = replaceMaxRec l y
    (ry, maxr) = replaceMaxRec r y

replaceMax:: BTree Int -> BTree Int
replaceMax bt = bt'
  where
    (bt', m) = replaceMaxRec bt m

--------------------------------------------------------------------------------
