-- Infinite list of all combinations for a given value domain
allCombinations :: [a] -> [[a]]
allCombinations []     = [[]]
allCombinations values = [] : concatMap (\w -> map (:w) values)
                                        (allCombinations values)
