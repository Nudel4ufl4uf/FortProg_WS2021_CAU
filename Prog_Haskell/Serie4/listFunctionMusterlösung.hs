import           SimplePrelude

--Eine Funktion, die eine Liste umdreht, kann man rekursiv definieren,
--indem man das erste Element einer nicht-leeren Liste hinter die umgedrehte Restliste hängt:

reverse1 :: [a] -> [a]
reverse1 []     = []
reverse1 (x:xs) = reverse1 xs ++ [x]

--Hier einige Beispielaufrufe getestet mit dem FortProg-GHCi:
{-
fortprog-ghci> reverse1 [1,2,3]
[3,2,1]
fortprog-ghci> reverse1 [1..10]
[10,9,8,7,6,5,4,3,2,1]
fortprog-ghci> reverse1 [10,8..1]
[2,4,6,8,10]
-}

--Mit Hilfe der Akkumulatortechnik kann man eine Liste schneller umkehren:
reverse2 :: [a] -> [a]
reverse2 l = rev l []
  where
    rev []     acc = acc
    rev (x:xs) acc = rev xs (x:acc)

--Hierbei hat jeder einzelne der n Schritte konstante Laufzeit. Die Gesamtlaufzeit
--ist also linear in der Länge n der gegebenen Liste.
--Zur Sicherheit ein paar Tests:
{-
fortprog-ghci> reverse2 [3,5,7]
[7,5,3]
fortprog-ghci> reverse2 [3,5..20]
[19,17,15,13,11,9,7,5,3]
fortprog-ghci> reverse2 [15,10..0]
[0,5,10,15]
-}

--Die Funktion indexOf bekommt als erstes Argument das Element, dessen Index in
--der gegebenen Liste (zweites Argument) sie berechnen soll. Falls das Element
--nicht in der Liste enthalten ist, ist das Ergebnis Nothing.

indexOf :: Int -> [Int] -> Maybe Int
indexOf = index 0
  where
    index _ _ []     = Nothing
    index i x (y:ys) = if x == y then Just i else index (i + 1) x ys

--Wir können das Ergebnis auch mit Hilfe eines case-Ausdrucks mittels Pattern Matching berechnen:
indexOf' :: Int -> [Int] -> Maybe Int
indexOf' _ []                 = Nothing
indexOf' x (y:ys) | x == y    = Just 0
                  | otherwise = case indexOf' x ys of
                                  Nothing -> Nothing
                                  Just i  -> Just (i + 1)

--Beide Funktionen haben dieselbe Laufzeit und liefern dieselben Ergebnisse:
{-
fortprog-ghci> indexOf 3 []
Nothing
fortprog-ghci> indexOf 3 [1,2,3]
Just 2
fortprog-ghci> indexOf 10 [1..9]
Nothing
fortprog-ghci> indexOf 5 [1,3,5,7,5,3,1]
Just 2
fortprog-ghci> indexOf' 3 []
Nothing
fortprog-ghci> indexOf' 3 [1,2,3]
Just 2
fortprog-ghci> indexOf' 10 [1..9]
Nothing
fortprog-ghci> indexOf' 5 [1,3,5,7,5,3,1]
Just 2
-}

add :: a -> [[a]] -> [[a]]
add x []       = []
add x (xs:xss) = (x:xs) : add x xss

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : add x (inits xs)

tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs

--Hier ein paar Tests
{-
fortprog-ghci> inits []
[[]]
fortprog-ghci> inits [1]
[[],[1]]
fortprog-ghci> inits [1,2,3]
[[],[1],[1,2],[1,2,3]]
fortprog-ghci> tails []
[[]]
fortprog-ghci> tails [1]
[[1],[]]
fortprog-ghci> tails [1,2,3]
[[1,2,3],[2,3],[3],[]]
-}

--Eine Funktion zum Einfügen eines Elements an einer beliebigen Position in einer
--Liste kann man rekursiv definieren. Das Ergebnis von insert ist eine Liste aller
--möglichen Listen, die nach dem Einfügen entstehen können.
insert :: a -> [a] -> [[a]]

--In die leere Liste kann man ein Element nur auf eine Art einfügen:
insert x []     = [[x]]

--In eine nicht-leere Liste kann man ein Element entweder vorne oder rekursiv weiter hinten einfügen.
--Nach dem rekursiven Einfügen muss man noch das ursprüngliche erste Element vorne an jedes mögliche
--Ergebnis anfügen. Auch hier können wir die bei inits eingeführte Funktion add verwenden.
insert x (y:ys) = (x:y:ys) : add y (insert x ys)

--Hier zwei Tests:
{-
fortprog-ghci> insert 42 []
[[42]]
fortprog-ghci> insert 1 [2,3]
[[1,2,3],[2,1,3],[2,3,1]]
-}

--Die Funktion insert kann man verwenden, um alle Permutationen einer Liste zu berechnen.
--Dazu fügt man das erste Element einer nicht-leeren Liste an einer beliebigen Stelle in jede
--Permutation der Restliste ein.
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = permInsert [] (perms xs)
  where
    permInsert acc []       = acc
    permInsert acc (ys:yss) = permInsert (acc ++ insert x ys) yss

--Ein paar Tests:
{-
fortprog-ghci> perms []
[[]]
fortprog-ghci> perms [1,2,3]
[[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
