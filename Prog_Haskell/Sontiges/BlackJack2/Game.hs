module Game where

import BlackJack
import Cards

blackJack :: IO ()
blackJack = do
  putStr header
  deck <- shuffle fullDeck
  gameLoop deck []

gameLoop :: Deck -> Hand -> IO ()
gameLoop deck player = do
  putStrLn ("Your current hand: " ++ concatMap pretty player)
  putStrLn ("Your current score: " ++ show (getValue player))
  if gameOver player
    then finish
    else do
      d <- yesNoQuestion "Draw another card?"
      if d then uncurry gameLoop (draw deck player)
           else finish
  where
  finish = do
    let bank = playBank deck
    putStrLn ("The bank's hand: " ++ concatMap pretty bank)
    putStrLn ("The bank's score: " ++ show (getValue bank))
    putStrLn ("The " ++ show (winner player bank) ++ " won.")
    again <- yesNoQuestion "Play again?"
    if again then blackJack else return ()

-- Ask a yes/no question and return True if the user input is 'y'
-- and False if it is 'n'
yesNoQuestion :: String -> IO Bool
yesNoQuestion question = do
  putStrLn (question ++ " (y)es | (n)o")
  c <- getChar
  putStrLn ""
  case c of
    'y' -> return True
    'n' -> return False
    _   -> putStrLn "Invalid input!" >> yesNoQuestion question

header :: String
header = unlines
  [ "------------------------------------------"
  , "| Welcome to Black Jack!                 |"
  , "| Beware! Gambling can ruin your life!!! |"
  , "| Call 7277 to get help.                 |"
  , "------------------------------------------"
  ]