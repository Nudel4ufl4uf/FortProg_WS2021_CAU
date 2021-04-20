import           SimplePrelude

--Wir beginnen mit dem Datentyp Tree eines Binärbaums mit Beschriftungen an den Blättern:
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Show

--Für einen Baum mit Zahlbeschriftungen kann man die Summe aller Knotenbeschriftungen rekursiv berechnen.
sumTree1 :: Tree Int -> Int
sumTree1 (Leaf     n) = n
sumTree1 (Branch l r) = sumTree1 l + sumTree1 r

--Die Version mit Akkumulatortechnik hat dieselbe Laufzeit (linear in der Knotenanzahl).
sumTree2 :: Tree Int -> Int
sumTree2 = sumTree 0
  where
    sumTree acc (Leaf     n) = n + acc
    sumTree acc (Branch l r) = sumTree (sumTree acc l) r

--------------------------------------------------------------------------------

--Bäume kann man rekursiv spiegeln und dabei immer linke und rechte Teilbäume vertauschen.
mirrorTree :: Tree a -> Tree a
mirrorTree (Leaf     n) = Leaf n
mirrorTree (Branch l r) = Branch (mirrorTree r) (mirrorTree l)

--------------------------------------------------------------------------------

--Für die Aufzählung der Beschriftungen bietet sich die Aufzählung von links nach rechts an.
toList :: Tree a -> [a]
toList (Leaf     n) = [n]
toList (Branch l r) = toList l ++ toList r
