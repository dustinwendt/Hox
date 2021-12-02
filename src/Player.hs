module Player where

import Card
import Colors

data Player = Player { playerId :: Int
                     , life :: Int
                     , library :: [Card]
                     , hand :: [Card]
                     , graveyard :: [Card]
                     , exile :: [Card]
                     , battlefield :: [Card]
                     , manaPool :: [ManaColor]
                     }
