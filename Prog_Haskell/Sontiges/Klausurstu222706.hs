
--------------------------------------------------------------------------
--3)
--1.
data Question = Question String [String] Int

question:: [Question]
question = [Question "Wie viele Farben hat die Flagge Deutschlands?" ["1","2","3"] 2,
            Question "Welche Farbe hat eine Bananen?" ["Rot","Gelb","Grün"] 1]

--2.
runAll :: [IO a] -> IO [a] 
runAll [] = return ()
runAll xs = do 
                list <- sequence xs
                return list

--3
askQuestion :: Question -> IO String
askQuestion Question q xs r = do 
                                --print the question
                                putStrLn q
                                if length xs >= 1 
                                    --print the possible answers
                                    then putStrLn $ printAn xs 0 
                                    --the user did not have enough answers
                                    else return ("You dont have enough answers!")
                                --get the Answer
                                ans <- getLine 
                                --check if the input is valid
                                
                                --the input is valid
                                putStrLn ans
                                --tell the user the input is not valid
                                putStrLn "Please choose a character between " ++ getLetter 1 ++ " and " getLetter 2 ++ "."
                                   
printAn :: [String] -> Int -> String
printAn [] _ = ""
printAn (x:xs) acc = getLetter ++ ": " ++ x ++ "\n" ++ printAn xs (acc+1)

getLetter :: Int -> String
--get the nth element from the alphabet
getLetter n = [x | x <- (["a" .. "z"] !! (n - 1)) ]

--4.
--main :: IO()
--main = do
    if helper then 

--helper :: Question -> IO Bool   
--helper Question q xs i = do 
--   a <- askQuestion q 
--    if a == i 
--        then return True
--       else return False             
--------------------------------------------------------------------------------------------------------------
--6)
--1.
data x a = xKons1 a 
         | xKons2 (Maybe (x a)) (Maybe a)
         | X a b 

--2.
foldX :: (a -> c) -> (Maybe c -> b -> Maybe c -> c) -> X a b -> c
foldX f xKons a

--3.
data BExp = Const Bool | Var String | And BExp BExp | Or BExp BExp | Not BExp

normalize :: BExp -> BExp 

--4.
type Valuation = [(String, Bool)]

allValuations :: [String] -> [Valuation]
allValuations [] = [(""),False] 
allValuations l = concatMap (\w -> map (:w) [True, False]) (allValuations l) 


----------------------------------------------------------------------------------------------------------------
--7)
--1.
vars :: BExp -> String
vars Const a = []
vars Var a = [a]
vars And exp1 exp2 = [vars exp1, vars exp2]
vars Or exp1 exp2 = [vars epx1, vars exp2]
vars Not exp1 = [vars exp1] 

value :: Valuation -> BExp -> Bool 
value [(s,b)] = 