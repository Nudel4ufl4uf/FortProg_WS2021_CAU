module Cards where

data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving Show

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Enum, Eq, Show)

data Card = Card Suit Rank
  deriving Show

-- Compute the value of a card regarding the rules of Black Jack
getCardValue :: Card -> Int
getCardValue (Card _ r) = value r
  where
  value (Numeric v) = v
  value Jack        = 10
  value Queen       = 10
  value King        = 10
  value Ace         = 11

-- A hand of cards is either empty or it is a card added to a hand of cards
type Hand = [Card]

-- Combine two hands of cards
(<+>) :: Hand -> Hand -> Hand
(<+>) = (++)

-- Return a deck of 52 cards
fullDeck :: Hand
fullDeck = [ Card s r
           | s <- [Clubs ..]
           , r <- map Numeric [2 .. 10] ++ [Jack, Queen, King, Ace]
           ]

-- Compute the number of aces in a hand of cards
numOfAces :: Hand -> Int
numOfAces = length . filter isAce
  where
  isAce (Card _ Ace) = True
  isAce _            = False

-- Compute the value of a hand of cards regarding the rules of Black Jack
getValue :: Hand -> Int
getValue h
  | val > 21  = val - 10 * numOfAces h
  | otherwise = val
  where val = sum (map getCardValue h)