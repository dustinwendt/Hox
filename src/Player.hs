module Player where

import Card
import Colors
import Zones

type PId = Int

data Player = Player { playerId :: PId
                     , life :: Int
                     , library :: Library
                     , hand :: Hand
                     , graveyard :: Graveyard
                     , exile :: Exile
                     , battlefield :: Battlefield
                     , manaPool :: [ManaColor]
                     }
