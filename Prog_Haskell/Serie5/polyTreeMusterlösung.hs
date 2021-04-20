import           SimplePrelude

data Tree a = Leaf a | Tree a :&: Tree a
  deriving Show

--Die flatTree-Funktion soll einen Baum von Bäumen zu einem einzigen Baum flachklopfen.
--Wir könnten sie rekursiv wie folgt definieren:
flatTreeRec :: Tree (Tree a) -> Tree a
flatTreeRec (Leaf  x) = x
flatTreeRec (s :&: t) = flatTreeRec s :&: flatTreeRec t

--Mit ein wenig Vorausschau können wir uns die Implementierung von flatTree erleichtern.
--Wählen wir beim Typ von extendTree für die Typvariable a den konkreten Typ Tree b,
--dann ergibt sich genau der Typ von flatTree. Aber nicht nur das, es ergibt sich auch die Implementierung:
flatTree :: Tree (Tree a) -> Tree a
flatTree = extendTree id

--Auch die mapTree-Funktion können wir entweder direkt oder mit Hilfe einer anderen Funktion implementieren.
--Hier die direkte Variante:
mapTreeRec :: (a -> b) -> Tree a -> Tree b
mapTreeRec f (Leaf  x) = Leaf (f x)
mapTreeRec f (s :&: t) = mapTreeRec f s :&: mapTreeRec f t

--Stattdessen können wir auch wieder extendTree verwenden, indem wir die übergebene
--Funktion mit dem Leaf-Konstruktor kombinieren:
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = extendTree (Leaf . f)

--Die Funktion extendTree können wir entweder direkt implementieren oder mit foldTree.
--Hier die direkte Implementierung:
extendTreeRec :: (a -> Tree b) -> Tree a -> Tree b
extendTreeRec f (Leaf  x) = f x
extendTreeRec f (s :&: t) = extendTreeRec f s :&: extendTreeRec f t

--Und hier die Variante mit foldTree:
extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f = foldTree f (:&:)

--Wenn wir die Implementierungen von flatTree und mapTree verwenden, die nicht auf extendTree basieren,
--können wir extendTree auch mit deren Hilfe definieren:
extendTree' :: (a -> Tree b) -> Tree a -> Tree b
extendTree' f = flatTreeRec . mapTreeRec f

--Die einzige Funktion, die wir direkt implementieren müssen, da wir sie nicht auf
--die anderen zurückführen können, ist foldTree. Sie bekommt für jeden Konstruktor
--des Tree-Datentyps ein Argument um diesen zu ersetzen:
foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree l _ (Leaf  x) = l x
foldTree l f (s :&: t) = foldTree l f s `f` foldTree l f t
