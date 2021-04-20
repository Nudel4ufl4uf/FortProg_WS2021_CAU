--------------------------------------------------------------------------------
--------------------------------- ##(1)## --------------------------------------
--This collection of functions creates a diamond of '*', with n + (n-1) standing
--for the size of the diamond.
--------------------------------------------------------------------------------

--build the dimond structure with n lines
diamond:: Int -> IO ()
diamond n = printD (buildD n)

--print the String List created by the builder below
printD:: [String] -> IO ()
printD []     = return ()
printD (x:xs) = do putStrLn x >> printD xs

--build the diamond in list form for n elements
buildD:: Int -> [String]
--mirror the created half of the diamond via map on the list
buildD n = mirror (map (line n) [0..n-1])

--build the half line with the appropraited n for each line and mirror it
line:: Int -> Int -> String
line n i = mirror (half n i)

--create the half of the needed line
half:: Int -> Int -> String
half n i = take (n-i-1) (repeat ' ') ++ "*" ++ take i (repeat ' ')

--mirror a given list
mirror:: [a] -> [a]
mirror xs = xs ++ tail (reverse xs)

--------------------------------------------------------------------------------
--------------------------------- ##(2)## --------------------------------------
--This collection of functions creates a diamond tree containing a half diamond
--with n lines and a root of the depth n-2.
--The print and mirror functions are the same in (1) and (2), thats why they are
--only writen in (1)!
--------------------------------------------------------------------------------

--build the dimond structure with n lines
diamondTree:: Int -> IO ()
diamondTree n = printD (buildDT n ++ buildR n)

--build the half diamond in list form for n elements
buildDT:: Int -> [String]
buildDT n = (map (lineT n) [0..n-1])

--build the half line with the appropraited n for each line and mirror it
lineT:: Int -> Int -> String
lineT n i = mirror (halfT n i)

--create the half of the needed line
halfT:: Int -> Int -> String
halfT n i = take (n-i-1) (repeat ' ') ++ "*" ++ take i (repeat '*')

--build the list of elements for the trees root
buildR:: Int -> [String]
buildR n = (map (root n) [0 .. n-2])

--create the list of elements of the trees root
root:: Int -> Int -> String
root n i = take (n-1) (repeat ' ') ++ "*"
