module GoblinKing where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


goblinKing = (properties.name .~ "Goblin King") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Goblin]) . (properties.oracleText .~ "Other Goblins get +1/+1 and have mountainwalk.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
