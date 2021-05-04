
--list the list in a new list and furthermore break it up as explianed in the task.
skips :: [a] -> [[a]]
skips xs = xs : skipping 2 xs
            where 
                skipping :: Int -> [a] -> [[a]]
                skipping n xs = [filter (\x -> (xs !! n ) mod n == 0) xs] : (if n <= length xs -2 
                                                                    then skipping (n + 1) xs 
                                                                    else [[xs !! (n + 1)]])
              

combine :: [a] -> [a] -> [[a]]
combine xs ys = [xs,ys]