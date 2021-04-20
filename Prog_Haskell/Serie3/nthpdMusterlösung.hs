import           SimplePrelude

pascal :: Int -> Int -> Int
pascal row pos = if pos == 0 || pos == row
  then 1
  else pascal (row - 1) (pos - 1) + pascal (row - 1) pos
