import           SimplePrelude

--Die map Funktion erhält die Listenstruktur und bildet nur die Elemente auf neue ab.
mapR :: (a -> b) -> [a] -> [b]
mapR f = foldr ((:) . f) []

--Aber wie kommt man auf Ausdrücke wie (:) . f? Indem man mit einer anonymen
--Funktion anfängt und diese systematisch umformt:
-- \x xs -> f x : xs
-- \x    -> (f x :)
-- \x    -> (:) (f x)
-- \x    -> ((:) . f) x
-- (:) . f

--Funktionen ohne explizite Argumente nennt man übrigens punktfrei.
--Auch die map Funktion kann man mit foldl nur umständlich definieren, und das
--führt zur gleichen Verschlechterung der Laufzeit wie bei append:
mapL :: (a -> b) -> [a] -> [b]
mapL f = foldl (\xs x -> xs ++ [f x]) []

--Die reverse-Funktion hingegen implementiert man am besten mit foldl.
--Interessanterweise entspricht die Definition mit foldr genau der naiven
--(ineffizienten) Variante und die Definition mit foldl der effizienten Implementierung:
-- quadratische Laufzeit
reverseR :: [a] -> [a]
reverseR = foldr (\x xs -> xs ++ [x]) []

-- lineare Laufzeit
reverseL :: [a] -> [a]
reverseL = foldl (\xs x -> x : xs) []
-- reverseL = foldl (flip (:)) []

--------------------------------------------------------------------------------

--unzip lässt sich recht leicht mit foldr implementieren, aber schwierig mit foldl:
unzipR :: [(a, b)] -> ([a], [b])
unzipR = foldr (\(a,b) (as,bs) -> (a : as, b : bs)) ([],[])

unzipL :: [(a, b)] -> ([a], [b])
unzipL = foldl (\(as,bs) (a,b) -> (as ++ [a], bs ++ [b])) ([],[])

--Die Variante mit foldr hat hier eine lineare Laufzeit bezogen auf die Listenlänge, die Variante mit foldl ist quadratisch.
--Bei nub verhält es sich ähnlich, wobei wegen des filter beide Implementierungen eine quadratische Laufzeit besitzen.
nubR :: [Int] -> [Int]
nubR ys = foldr (\x xs -> x : filter (x/=) xs) [] ys

nubL :: [Int] -> [Int]
nubL ys = foldl (\xs x -> filter (x/=) xs ++ [x]) [] ys
