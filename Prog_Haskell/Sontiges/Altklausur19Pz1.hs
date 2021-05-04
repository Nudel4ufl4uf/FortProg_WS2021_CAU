import Data.Char 
data Move = Schere | Stein | Papier

instance Ord Move where 
    compare m1 m2 = case of compare m1 m2 of
        

getMove :: IO Move
getMove = do c <- getChar
            if c == 'Schere'  then return Schere
                else if c == 'Stein' then return Stein
                    else if c == 'Papier' then return Papier
                        else getMove
                
