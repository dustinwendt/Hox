module Player where

import Card
import Colors
import Zones

data Player = Player { playerId :: PId
                     , life :: Int
                     , library :: Library
                     , hand :: Hand
                     , graveyard :: Graveyard
                     , manaPool :: [ManaColor]
                     }
