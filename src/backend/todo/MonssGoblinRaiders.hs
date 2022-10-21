module MonssGoblinRaiders where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


monssGoblinRaiders = (properties.name .~ "Mons's Goblin Raiders") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Goblin]) . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
