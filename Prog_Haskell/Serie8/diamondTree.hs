

--build the dimond structure with n lines
diamondTree:: Int -> IO ()
diamondTree n = printD (buildD n ++ buildR n)

--print the String List created by the builder below
printD :: [String] -> IO ()
printD []     = return ()
printD (x:xs) = do putStrLn x >> printD xs

--build the diamond in list form for n elements
buildD:: Int -> [String]
buildD n = (map (line n) [0..n-1])

--build the half line with the appropraited n for each line and mirror it
line:: Int -> Int -> String
line n i = mirror (half n i)

--create the half of the needed line
half:: Int -> Int -> String
half n i = take (n-i-1) (repeat ' ') ++ "*" ++ take i (repeat '*')

--mirror a given list
mirror:: [a] -> [a]
mirror xs = xs ++ tail (reverse xs)

--build the list of elements for the trees root
buildR:: Int -> [String]
buildR n = (map (root n) [0 .. n-2])

--create the list of elements of the trees root
root:: Int -> Int -> String
root n i = take (n-1) (repeat ' ') ++ "*"
