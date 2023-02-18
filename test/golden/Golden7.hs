module Golden7 where

import Data.List (sortOn)
import Data.Coerce

data Rank = Ace | Two | Three
          | Four | Five | Six
          | Seven | Eight | Nine
          | Ten | Jack | Queen
          | King deriving (Show, Eq, Ord, Enum, Bounded)

-- newtype Card = Card { unCard :: (Rank, Suit) } deriving (Show, Eq, Ord)
type Card = (Rank, Suit)

data Suit = Spades | Diamonds | Hearts | Clubs
  deriving (Show, Eq, Ord, Enum, Bounded)


-- allCards :: [Card]
allCards :: [Card]
allCards = (,) <$> [minBound ..] <*> [minBound ..]

-- allCards = coerce $ (,) <$> [minBound @Rank .. maxBound] <*> [minBound @Suit .. maxBound]

main :: IO ()
main = undefined
