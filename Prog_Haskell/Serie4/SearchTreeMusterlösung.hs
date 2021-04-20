import           SimplePrelude

--Wir beginnen mit der Definition eines Suchbaums. Ein Suchbaum ist entweder leer
--oder es handelt sich um einen Zweig mit einer ganzzahligen Knotenbeschriftung
--und einem linken und rechten Teilbaum.

data SearchTree = Empty | Branch SearchTree Int SearchTree
  deriving Show

--Die insert-Funktion fügt ein Element nur ein, wenn es noch nicht vorhanden ist.
--In dem Fall wird für das Element ein neues Blatt erzeugt und so in den Baum gehängt,
--dass es rechts von kleineren und links von größeren Elementen steht.

insert :: Int -> SearchTree -> SearchTree
insert x Empty          = Branch Empty x Empty
insert x (Branch l n r)
  | x == n              = Branch l n r
  | x <  n              = Branch (insert x l) n r
  | otherwise           = Branch l n (insert x r)

--Die Funktion isElem nutzt die Ordnung des Baums, um die richtige Beschriftung zu finden:

isElem :: Int -> SearchTree -> Bool
isElem _ Empty          = False
isElem x (Branch l n r)
  | x == n              = True
  | x <  n              = isElem x l
  | otherwise           = isElem x r

--eine Funktion splitMin, die die kleinste Beschriftung eines sortierten Baumes
--und den Restbaum als Ergebnis liefert. Da Funktionen nur ein Ergebnis liefern können,
--liefern wir ein Paar aus beiden Teilen:

splitMin :: SearchTree -> (Int, SearchTree)
splitMin Empty              = error "splitMin: empty search tree"
splitMin (Branch Empty n r) = (n, r)
splitMin (Branch l     n r) = (m, Branch t n r)
  where (m, t) = splitMin l

--Die splitMin Funktion ist partiell, da sie nur für nicht-leere Bäume definiert ist.
--Sie wird aber auch nur auf solchen aufgerufen.
--Mit ihrer Hilfe können wir delete implementieren. Da jede Beschriftung höchstens
--einmal im Baum vorkommt, genügt es, die erste gefundene zu entfernen.

delete :: Int -> SearchTree -> SearchTree
delete _ Empty              = Empty
delete x (Branch Empty n r)
  | x == n    = r
  | x <  n    = Branch Empty n r
  | otherwise = Branch Empty n (delete x r)
delete x (Branch l n Empty)
  | x == n    = l
  | x <  n    = Branch (delete x l) n Empty
  | otherwise = Branch l            n Empty
delete x (Branch l n r)
  | x == n    = Branch l            m t
  | x <  n    = Branch (delete x l) n r
  | otherwise = Branch l            n (delete x r)
  where (m, t) = splitMin r
