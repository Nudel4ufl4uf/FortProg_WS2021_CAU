
------------------------------------------------------------------------------
--hochgeladene Lösung

import           Prelude

-- data structure for rose trees
data Rose a = Rose a [Rose a]

-- test cases
testRose1 :: Rose Int
testRose1 = Rose 1 [Rose 2 [Rose 3 []]]

testRose2 :: Rose Int
testRose2 = Rose 1 [Rose 2 [Rose 3 []]]

testRose3 :: Rose Int
testRose3 = Rose 1 []

prettyTest :: Rose Int
prettyTest = Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []]

-- instance of type class Eq for rose trees
instance Eq (a) => Eq (Rose a) where
  Rose a1 []   == Rose a2 []   = a1 == a2
  -- when there are rose trees left, check them for equality too
  Rose a1 [r1] == Rose a2 [r2] = a1 == a2 && r1 == r2
  -- else the two rose trees aren't equal
  _            == _            = False

-- instace of type class Ord for rose trees
instance Ord (a) => Ord (Rose a) where
  -- in this case the trees are equal in size
  compare (Rose a1 []) (Rose a2 [])     = EQ
  -- in this case the first tree is greater in size
  compare (Rose a1 [r1]) (Rose a2 [])   = GT
  -- in this case the first tree is smaller in size
  compare (Rose a1 []) (Rose a2 [r2])   = LT
  -- when there are rose trees left to check, compare them
  compare (Rose a1 [r1]) (Rose a2 [r2]) = compare r1 r2

class Pretty a where
  pretty:: a -> String

--method to print the RoseTree in the given format
instance (Show a) => Pretty (Rose a) where
  --case for an element with no leafs -> to be drawn
  pretty (Rose n [])     = "+-- " ++ show n ++ "\n"
  --case for an element with i leafs -> to be rekursively used until no leafs
  pretty (Rose n (x:xs)) = "|   " ++ show (map (pretty) xs)
