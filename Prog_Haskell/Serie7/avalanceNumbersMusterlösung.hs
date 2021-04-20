--Eine naive, sehr langsame Implementierung lässt sich mittels filter definieren.
avalancheNaive :: [Integer]
avalancheNaive = filter isAvalanche [1 ..]
  where isAvalanche 1 = True
        isAvalanche n | n `mod` 7 == 0 = isAvalanche (n `div` 7)
        isAvalanche n | n `mod` 5 == 0 = isAvalanche (n `div` 5)
        isAvalanche n | n `mod` 3 == 0 = isAvalanche (n `div` 3)
        isAvalanche _ = False

--Um alle Avalanche-Zahlen zu erzeugen, reicht es also aus für jede bekannte Zahl
--m die Vielfachen 3 m 5 m und 7 m zu berechnen und diese zu sortieren.
avalanche :: [Integer]
avalanche = 1 : map (3 *) avalanche
        `merge` map (5 *) avalanche
        `merge` map (7 *) avalanche

merge :: [Integer] -> [Integer] -> [Integer]
merge []     ns     = ns
merge ms     []     = ms
merge (m:ms) (n:ns) = case compare m n of
  LT -> m : merge ms (n:ns)
  EQ -> m : merge ms ns
  GT -> n : merge (m:ms) ns
