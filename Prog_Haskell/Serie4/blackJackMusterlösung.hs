import SimplePrelude

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
Nun können wir die Funktionen wie folgt definieren.


-- Return a deck of 52 cards
fullDeck :: Hand
fullDeck = cardsOfSuit Clubs  <+> cardsOfSuit Spades <+>
           cardsOfSuit Hearts <+> cardsOfSuit Diamonds
  where
  cardsOfSuit s = numCards [2,3,4,5,6,7,8,9,10] <+>
                  (Add (Card s Jack)
                  (Add (Card s Queen)
                  (Add (Card s King)
                  (Add (Card s Ace) Empty))))
    where
    numCards []     = Empty
    numCards (n:ns) = Add (Card s (Numeric n)) (numCards ns)

-- Compute the number of aces in a hand of cards
numOfAces :: Hand -> Int
numOfAces Empty = 0
numOfAces (Add c h) = if isAce c then 1 + numOfAces h else numOfAces h
  where
  isAce (Card _ Ace) = True
  isAce _            = False

-- Compute the value of a hand of cards regarding the rules of Black Jack
getValue :: Hand -> Int
getValue h = let val = getValue' h -- we use a let binding here to prevent code duplication
             in if val > 21 then val - 10 * numOfAces h else val
  where
  getValue' Empty     = 0
  getValue' (Add c r) = getCardValue c + getValue' r

hand1 :: Hand
hand1 = Add (Card Hearts Ace) (Add (Card Diamonds Ace) (Add (Card Spades Queen) Empty))
