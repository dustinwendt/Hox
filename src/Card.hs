module Card where
import Types

-- 208 Power/Toughness
data StarNum = Star | StarPlus Int
data Power = PowStar StarNum | PowNum Int
data Toughness = TouStar StarNum | TouNum Int

data Card = Card { cardName :: String
                 , manaCost :: Maybe [Pip]
                 , color :: [Color]
                 , typeLine :: TypeLine
                 , textBox :: String
                 , power :: Maybe Power
                 , toughness :: Maybe Toughness
                 , loyalty :: Maybe Int
                 , handMod :: Maybe Int
                 , lifeMod :: Maybe Int}

type Deck = [Card]

defaultCard :: Card
defaultCard = Card { cardName = "DefaultCard"
                   , manaCost = Nothing
                   , color = []
                   , typeLine = TypeLine [] [] []
                   , textBox = ""
                   , power = Nothing
                   , toughness = Nothing
                   , loyalty = Nothing
                   , handMod = Nothing
                   , lifeMod = Nothing
                   }

-- 202.3
manaValue :: Card -> Int
manaValue c = case c of
  Card { manaCost = Just pips} -> go pips
  _ -> 0
  where go [] = 0
        go (Generic x : xs) = x + go xs
        go (VarPip : xs) = go xs
        go (_:xs) = 1 + go xs

isMonoColored :: Card -> Bool
isMonoColored c = case c of
  Card {color = x} -> length x == 1

isMultiColored :: Card -> Bool
isMultiColored c = case c of
  Card {color = x} -> length x > 1

isColorless :: Card -> Bool
isColorless c = case c of
  Card {color = x} -> null x

