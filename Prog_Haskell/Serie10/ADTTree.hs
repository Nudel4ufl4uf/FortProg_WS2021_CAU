import Test.QuickCheck

--4.1)
data Tree = Leaf Int | Node Tree Tree
  deriving (Eq, Show)

size :: Tree -> Int
size (Leaf x) = 1
size (Node tL tR) = (size tL) + (size tR)


toList :: Tree -> [Int]
toList tree = case tree of
    (Leaf l)         -> l:[]
    (Node t1 t2)     -> toList t1 ++ toList t2

--4.2)
prop_toList :: Tree -> [Int] -> Bool
prop_toList t xs = 
  let xs = toList t
  in size t == length xs

instance Arbitrary (Tree) where
  arbitrary = sized $ \n -> do  k <- choose (2,n)
                                genTree k

genTree :: Int -> Gen (Tree)
genTree n | n > 0 = do
            tl <- genTree (div n 2)
            tr <- genTree (div n 2)
            return (Node tl tr)
          | otherwise = do
            l <- arbitrary
            return (Leaf l)

    