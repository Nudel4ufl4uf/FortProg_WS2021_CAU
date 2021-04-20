import           SimplePrelude

-- computes the number of the "Pascalsche Dreick" in row n and pos main
-- pascal row pos is the sum of the two numbers above
pascal :: Int -> Int -> Int
pascal row pos = if pos == 0 || pos == row then 1
                                           else pascal (row - 1) (pos - 1) + pascal (row - 1) (pos)
