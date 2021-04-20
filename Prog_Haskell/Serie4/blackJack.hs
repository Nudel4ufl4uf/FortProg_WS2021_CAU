import           SimplePrelude

data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving Show

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving Show

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
data Hand = Empty
          | Add Card Hand
  deriving Show

-- Combine two hands of cards
(<+>) :: Hand -> Hand -> Hand
Empty      <+> h2 = h2
(Add c h1) <+> h2 = Add c (h1 <+> h2)

-- Returns a deck of 52 cards
fulldeck :: Hand
fulldeck = let
            -- Helper function to build the deck part for each suit
            buildDeckParts :: Suit -> Hand
               -- Adds card after card for the given suit s
            buildDeckParts s = Add (Card s (Numeric 2)) Empty <+>
                               Add (Card s (Numeric 3)) Empty <+>
                               Add (Card s (Numeric 4)) Empty <+>
                               Add (Card s (Numeric 5)) Empty <+>
                               Add (Card s (Numeric 6)) Empty <+>
                               Add (Card s (Numeric 7)) Empty <+>
                               Add (Card s (Numeric 8)) Empty <+>
                               Add (Card s (Numeric 9)) Empty <+>
                               Add (Card s (Numeric 10)) Empty <+>
                               Add (Card s Jack) Empty <+>
                               Add (Card s Queen) Empty <+>
                               Add (Card s King) Empty <+>
                               Add (Card s Ace) Empty
          -- Finally builds the whole deck by using biuldDeckParts for each suit
          in buildDeckParts Clubs <+> buildDeckParts Spades <+>
             buildDeckParts Hearts <+> buildDeckParts Diamonds

-- Returns the number of aces in a hand
numOfAces :: Hand -> Int
-- Case: A wild ace appears -> Count 1 and continue with the rest of the hand
numOfAces (Add (Card _ Ace) hs) = 1 + numOfAces hs
-- Case: Card isn't an ace, so just continue with the rest of the hand
numOfAces (Add (card) hs)       = numOfAces hs
-- Hand is empty initally, so there can't be any aces
numOfAces Empty                 = 0

-- Calculates the value of a whole hand of cards
getValue :: Hand -> Int
getValue hand = let
                 -- Helper function that uses getCardValue to sum up
                 sumF :: Hand -> Int
                 sumF Empty         = 0
                 sumF (Add card hs) = getCardValue card + sumF hs
                 s = sumF hand
                -- Additional condition: If overall value > 21 then all aces only count as 1 point
                in if s > 21 then s - numOfAces hand * 10
                             else s
