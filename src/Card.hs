module Card where

import Colors
import Types

-- 208 Power/Toughness
data PT = Star | StarPlus Int | PT Int
-- data Power = PowStar StarNum | PowNum Int
-- data Toughness = TouStar StarNum | TouNum Int

data Card = Card { cardName :: String
                 , manaCost :: [Pip]
                 , color :: [Color]
                 , identity :: [Color]
                 , typeLine :: TypeLine
                 , textBox :: String
                 , power :: Maybe PT
                 , toughness :: Maybe PT
                 , loyalty :: Maybe Int
                 , token :: Bool
                 , copy :: Bool
                 , owner :: Int
                 }

type Deck = [Card]

defaultCard :: Card
defaultCard = Card { cardName = "DefaultCard"
                   , manaCost = []
                   , color = []
                   , identity = []
                   , typeLine = TypeLine [] [] []
                   , textBox = ""
                   , power = Nothing
                   , toughness = Nothing
                   , loyalty = Nothing
                   , token = False
                   , copy = False
                   , owner = -1
                   }

-- 202.3
pipValue :: Pip -> Int
pipValue x = case x of
              HyPip a b -> if v a > v b then v a else v b
              p -> v p
           where v XSym       = 0
                 v (GenSym i) = i
                 v _          = 1

manaValue :: Card -> Int
manaValue c = case c of
  Card { manaCost = pips} -> foldr ((+) . pipValue) 0 pips

isMonoColored :: Card -> Bool
isMonoColored c = case c of
  Card {color = x} -> length x == 1

isMultiColored :: Card -> Bool
isMultiColored c = case c of
  Card {color = x} -> length x > 1

isColorless :: Card -> Bool
isColorless c = case c of
  Card {color = x} -> null x

