module BlackJack where

import           Data.List     (splitAt)
import           Data.Maybe    (fromJust)
import           System.Random


-- Representation of Black Jack players
data Player = Guest | Bank
  deriving Show

type Deck = [Card]

-- Check if the game is over, i.e. the value of the hand is greater than 21
gameOver :: Hand -> Bool
gameOver h = getValue h > 21

-- Determine which player won
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest || getValue guest <= getValue bank && not (gameOver bank) = Bank
  | otherwise                                                                = Guest

-- Shuffle a deck of cards
shuffle :: Deck -> IO Deck
shuffle deck = newStdGen >>= return . shuffle' deck []
  where
    shuffle' [] h _ = h
    shuffle' cs h g = let (i  , g'    ) = randomR (0, length cs - 1) g
                          (cs', c:cs'') = splitAt i cs
                      in shuffle' (cs' ++ cs'') (c:h) g'

-- Draw a card from a deck and put it on the given hand
draw :: Deck -> Hand -> (Deck, Hand)
draw []     _ = error "Game.draw: The deck is empty"
draw (c:cs) h = (cs, c:h)

-- The bank draws cards from the deck until a value of at least 16 is reached
playBank :: Deck -> Hand
playBank deck = playBank' deck []
  where
    playBank' d h | getValue h >= 16 = h
                  | otherwise        = uncurry playBank' (draw d h)

-- -----------------------------------------------------------------------------
-- Pretty printing of cards
-- -----------------------------------------------------------------------------

class Pretty a where
  pretty :: a -> String

-- slightly hackish Pretty instance
instance Pretty Card where
  pretty (Card s r) = toEnum (fromJust (lookup s suitMap) + offset r) : " "
    where
      suitMap            = [ (Spades, 127137), (Hearts, 127153)
                          , (Diamonds, 127169), (Clubs, 127185) ]
      offset (Numeric n) = n - 1
      offset Jack        = 10
      offset Queen       = 12
      offset King        = 13
      offset Ace         = 0

-- -----------------------------------------------------------------------------
-- Functionality extracted from Cards.hs
--------------------------------------------------------------------------------

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

-- Combine two decks of cards
(<+>) :: Deck -> Deck -> Deck
(<+>) = (++)

-- Return a deck of 52 cards
fullDeck :: Deck
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