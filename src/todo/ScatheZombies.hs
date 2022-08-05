module ScatheZombies where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


scatheZombies = (properties.name .~ "Scathe Zombies") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Zombie]) . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
