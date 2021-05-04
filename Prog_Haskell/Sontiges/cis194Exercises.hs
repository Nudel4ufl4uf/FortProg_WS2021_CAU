--1) CreditCard Exercise

--double every second value begining at the right
doubleValues :: [Int] -> [Int]
doubleValues xs = revV (reverse xs) 
    where 
        revV (x:y:[]) = [x] ++ [if y*2 > 9 then y*2 - 9 else y*2]
        revV (x:[]) = [x] 
        revV (x:y:xs) = reverse ([x] ++ [if y*2 > 9 then y*2-9 else y*2] ++ revV (reverse xs)) 

--check the sum of the processed numbers
checkSum :: [Int] -> Int 
checkSum xs = foldr1 (+) xs

--checks if the remainder is divideble by 10
remainder:: Int -> Int 
remainder i = mod i 10

--whole card check method
checkValid :: [Int] -> Bool
checkValid xs = if (remainder (checkSum (doubleValues xs))) == xs !! (length xs - 1) then True else False