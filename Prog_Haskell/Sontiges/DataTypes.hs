import SimplePrelude

-- Enumeration types:
--data Bool = False | True

data Color = Red | Blue | Yellow

colorValue :: Color -> Int
colorValue Red    = 0
colorValue Blue   = 1
colorValue Yellow = 2

-- Record types: Complex number
data Complex = Complex Float Float

-- Complex 3.14 2.7 :: Complex

-- Addition on complex numbers:
addC :: Complex -> Complex -> Complex
addC (Complex r1 i1) (Complex r2 i2) = Complex (r1 +. r2) (i1 +. i2)


-- Integer lists or sequences:
data IntList = Nil | Cons Int IntList
 deriving Show -- automatically define a "show" function for IntList

list12 :: IntList
list12 = Cons 1 (Cons 2 Nil)

list345 :: IntList
list345 = Cons 3 (Cons 4 (Cons 5 Nil))

-- Concatenate two list to a list containing all elements:
append :: IntList -> IntList -> IntList
--append xs ys = ???
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

-- Predefined in Haskell as [Int]
-- data [Int] = [] | Int : [Int]
-- ":": right-associative infix operator

hlist12 :: [Int]
hlist12 = 1 : (2 : [])

hlist345 :: [Int]
--hlist345 = 3 : (4 : (5 : []))
--hlist345 = 3 : 4 : 5 : []
hlist345 = [3, 4, 5]

-- append is predefined as "++" (infix operator!)
--hlist12345 = hlist12 ++ hlist345
hlist12345 = (++) hlist12 hlist345
