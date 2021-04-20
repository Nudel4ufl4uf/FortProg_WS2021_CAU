--Abgabe Philo Klemenz (stu222706)
---------------------------------------------------------------------------------------------
--3)
--1.
data Grade = A | B | C | D | E
  deriving (Eq, Show)

type Name = String
type Points = Int

data Submission = Sub Name Points
data Module = FPP | FP8 | FPK | EFP
data AttendanceList = AList [(Name, Module)]

--example instance for the "Abgabelist"
subs :: [Submission]
subs = [Sub "Peter Lustig" 10, Sub "Helge Schneider" 7, Sub "Mirko Boland" 3]

--example instance for the "Anwesenheitsliste"
alist :: AttendanceList
alist = AList[("Peter Lustig", FPP),("Helge Schneider", FP8), ("Mirko Boland", FPK)]

--2.
maxPoints :: Module -> Points
maxPoints m = case m of
                FPP -> 50
                FP8 -> 50
                FPK -> 40
                EFP -> 28
            
grade :: Points -> Points -> Grade
grade points max = grade' (toEnum points / toEnum max)
  where grade' p | p >= 0.875             = A
                 | p < 0.875 && p >= 0.75 = B
                 | p < 0.75 && p >= 0.625 = C
                 | p < 0.625 && p >= 0.5  = D
                 | p < 0.5                = E

gradeSubmissions :: [Submission] -> AttendanceList -> [(Submission, Grade)]
--if there is no Attendance left the submission is invalid and doesnt count
gradeSubmissions _ [] = []
--get the the Submission and the maxGrade for the specific Module and go on for the rest Submissions
gradeSubmissions (x:xs) (y:ys) = [(x,grade (snd x) (maxPoints(snd y)))] ++ gradeSubmissions xs ys 

--3.
--create a Notenspiegel based on the given Grades; only ammounts over 0 are showen
gradeDistribution :: [Grade] -> [(Grade, Int)]
gradeDistribution xs = count [] xs
                        where 
                            count:: [Grade] -> [Grade] -> [Grade] 
                            --stop if all grades are checked
                            count _ [] = []
                            --count number of grades
                            count acc (x:xs)= if (length . filter (== x)) xs > 0 
                                                --the ammount > 0 means its relevant
                                                then acc : ([(x, ammount)] ++ gradeDistribution xs) 
                                                --not saving irrelevant ammounts
                                                else count acc xs


--4.
publishGrades :: [Submission] -> AttendanceList -> IO ()
publishGrades xs aL = do 
                        let sub = gradeSubmissions xs aL
                            nS = gradeDistribution (createGradeList sub)
                        printGrades nS 
                        return()

createGradeList:: [(Submission,Grade)] -> [Grade]
createGradeList (x:xs) = snd x : createGradeList xs

printGrades :: [(Grade, Int)] -> IO()
printGrades xs = do 
                    mapM (\(x1,x2) -> putStrLn ("Grade: " ++ x1 ++ " = " ++ x2 ++ " Times")) xs 
                    return ()

---------------------------------------------------------------------------------------------------------------------------------
--6)
--1.
data Number = Num [()]
  deriving (Show, Eq)

--1.
--Es kann keine Functor Instance des "Data types" Number erstellt werden da es sich bei Num um keine konkreten Datentyp handelt.
--instance Functor Number where
--    fmap f [()] = [f()]

--2. 
add :: Number -> Number -> Number
--add the two list together to get the sum of both
add xs ys = xs ++ ys

--3.
mult :: Number -> Number -> Number
mult xs ys = map (++) xs ys 

--4.
subt :: Number -> Number -> Maybe Number
subt [] _ = Nothing
subt (x:xs) (y:ys) = if x == y then Just [] ++ subt xs ys else Just xs

--5.

--6. 
pow2 :: [Number]
pow2 xs = xs : map (mult xs) pow2 xs 

-----------------------------------------------------------------------------------------------------------------------------------
--7)
--1.
instance Ord Number where
    compare (Num n1) (Num n2) = compare n1 n2

--2.
modulo :: Number -> Number -> Number
modulo n1 n2 
            | subt n1 n2 >= n1 = modulo (subt n1 n2) n2
            | n1 < n2          = n1
            | subt n1 n2 < n1  = subt n1 n2

--3.
fromNumber :: Number -> Int 
fromNumber n = length n

toNumber :: Int -> Maybe Number
toNumber i = if i <= 0 
            then Just (Num [()] : if i-1 >= 0 
                then toNumber i-1 
                else [] ) 
            else Nothing