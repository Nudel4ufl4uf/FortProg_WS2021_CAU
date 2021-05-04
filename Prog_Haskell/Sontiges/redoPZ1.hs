data Question = Question String [String] Int

questions :: [Question]
questions = [Question "Welche Farbe hat eine Banane?" ["Grün", "Gelb", "Schwarz", "Rot"] 2,
            Question "Wie viele Ringe hat das Audi Logo?" ["1", "2", "3", "4"] 4]

testquestion :: Question
testquestion = Question "Wie viele Ringe hat das Audi Logo?" ["1", "2", "3", "4"] 4

runAll :: [IO a] -> IO [a]
runAll [] = return []
runAll (x:xs) = do
            stuff <- x
            rest <- runAll xs
            return (stuff:rest)

askQuestion :: Question -> IO String
askQuestion  q@(Question question answers _) = do
    putStrLn question
    let answerPairs = zip ['a' .. 'z'] answers
    runAll (map(\(c,str) -> putStrLn (c : ": " ++ str)) answerPairs)
    c <- getChar 
    putStrLn ""
    case lookup c answerPairs of
        Nothing -> 
            let finalChar = ['a' .. 'z'] !! (length answers - 1)
            in putStrLn ("Please choose a character btween 'a' and " ++ show finalChar)
            >> askQuestion q
        Just str -> return str 

ask :: Question -> IO Bool 
ask q@(Question question answers solutionIndex) = do 
    answer <- askQuestion q
    return (answers !! solutionIndex == answer)

main :: IO()
main = do
    results <- runAll (map ask questions)
    let correctAnswers = foldr (\b cnt -> if b then cnt + 1 else cnt) 0 results
    putStrLn ("You got " ++ show correctAnswers ++ " out of " ++ show (length questions) ++ " correct.")

---------------------------------------------------------------------------------------------------------------------------
--2)
--1.
data X a b
  = C1 a
  | C2 (Maybe (X a b)) b (Maybe (X a b))

--2.
foldX :: (a -> c) -> (Maybe c -> b -> Maybe c -> c) -> X a b -> c
foldX f _ (C1 x) = f x
foldX f g (C2 m1 x m2) = g (fmap (foldX f g) m1) x (fmap (foldX f g) m2)

--3.
data BExp = Const Bool | Var String | And BExp BExp | Or BExp BExp | Not BExp

--normalize :: BExp -> BExp 
--normalize (Not(And e1 e2)) = Or (Not e1) (Not e2)
--normalize (Not(Or e1 e2))  = And (Not e1) (Not e2)
--normalize (Not (Not e))   = e 

normalize :: BExp -> BExp
normalize (And b1 b2) = And (normalize b1) (normalize b2)
normalize (Or b1 b2) = Or (normalize b1) (normalize b2)
normalize (Not (Var x)) = Not (Var x)
normalize (Not b) = normalize (negate b)
  where
    negate (Const b) = Const (not b)
    negate (Var i) = Not (Var i)
    negate (And b1 b2) = Or (negate b1) (negate b2)
    negate (Or b1 b2) = And (negate b1) (negate b2)
    negate (Not b) = b
normalize b = b

type Valuation = [(String, Bool)]

allValuations :: [String] -> [Valuation]
allValuations xs = mapM (\x -> [(x,True),(x,False)]) xs


--------------------------------------------------------------------------------------------------------------
--3)
--1.
vars :: BExp -> [String] 
vars exp = createString [] exp  
    where 
        createString :: [String] -> BExp -> [String]
        createString acc (Var v) = acc ++ [v]
        createString acc (And e1 e2) = createString acc e1 ++ createString acc e2  
        createString acc (Or e1 e2) = createString acc e1 ++ createString acc e2
        createString acc (Not e) = createString acc e  
        createString acc (Const _) = []

value :: Valuation -> BExp -> Bool
value _ (Const b) = b
value v (Var i) = case lookup i v of
  Just b -> b
  Nothing -> error ("Missing variable " ++ show i)
value v (And b1 b2) = value v b1 && value v b2
value v (Or b1 b2) = value v b1 || value v b2
value v (Not b) = not (value v b)

satisfiable :: BExp -> [Valuation]
satisfiable b = filter (\x -> value x b) (allValuations (vars b))

