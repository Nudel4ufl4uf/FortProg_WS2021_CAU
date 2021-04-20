import SimplePrelude

-- Some test lists
testList1 :: [Int]
testList1 = [0, 1, 2]

testList2 :: [Int]
testList2 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- Reverses a list (polymorph datatype)
reverse1 :: [a] -> [a]
reverse1 [] = []
-- Concatenate the list from back to front by putting the first element at the back
-- and do this recursively for the rest
reverse1 (x : xs) = reverse1 xs ++ [x]

{-
Main> reverse1 testList1
[2,1,0]
(0.00 secs, 126,400 bytes)
Main> reverse1 testList2
[10,9,8,7,6,5,4,3,2,1,0]
(0.00 secs, 142,160 bytes)
-}

-- Reverses a list but with accumulation
reverse2 :: [a] -> [a]
-- Concatenates with an empty list as starting list and any list xs
reverse2 xs = reverseAcc [] xs
 where
   -- Case: There is nothing left to reverse
   reverseAcc ys []       = ys
   -- Puts the the first element into the initally empty list, then continues to do
   -- so for the rest of the list which finally reverses it
   reverseAcc ys (x : xs) = reverseAcc (x : ys) xs

-- Returns the index of the first appearance of a given Int
indexOf :: [Int] -> Int -> Maybe Int
-- Starts with the 0.th Element, takes the list of Ints and the number to look for
indexOf xs number = indexOfAcc 0 xs number
  where
    -- Case: There is nothing to check left and the number hasn't been found
    indexOfAcc _ [] _ = Nothing
    -- Case: Is the x we look at the number we looked for? Otherwise it may be the
    -- next one -> increase the index count by one and go on with xs
    indexOfAcc index (x : xs) number | x == number    = Just index
                                     | otherwise = indexOfAcc (index + 1) xs number

-- Calculates the beginning pieces of a list
inits :: [a] -> [ [a] ]
-- Case: Empty list left -> no pieces to calculate anymore, just put the empty list in
inits [] = [ [] ]
-- Put the list itself in the back and use inits on the list but without the last
-- element (with predefined take function in combination with predefined length function)
-- and go on with this procedure until the empty list is added
inits (xs) = inits (take ((length xs) - 1) xs) ++ [xs]

-- Caclulates the ending pieces of a list
tails :: [a] -> [ [a] ]
-- Case: Empty list left -> no pieces to calculate anymore, just put the empty list in
tails []       = [ [] ]
-- Put the list itself at the front and and use tails on the list but without the first
-- element and go on with this procedure until the empty this is added
tails (x : xs) = [x : xs] ++ tails xs

-- Inserts a given element into every place in a list
insert :: a -> [a] -> [ [a] ]
insert newelem [] = [ [newelem] ]
-- Insert newelem into xs with the helper function insertAcc starting at the index 0
insert newelem xs = insertAcc 0 newelem xs
 where
   -- Helper function that takes an index, an elemnt to insert and the list
                          -- Case: The index where to place the new element hasn't been reached yet -> we can construct the new list
                          -- by concatenating the part of the list from place 0 to place index (with predefined function take) ++
                          -- the element to insert ++ the rest of the list initially appearing after the place index (with predefined
                          -- function drop (couldn't find any way on how to solve this with a function from the lecture))
   insertAcc index new ys | index < length ys = [(take index ys) ++ [newelem] ++ (drop index ys)] ++ insertAcc (index + 1) new ys
                          -- Otherwise the index reached the end and the last possibility to insert the new element
                          | otherwise = [ys ++ [newelem]]

--perms :: [a] -> [ [a] ]
--perms
