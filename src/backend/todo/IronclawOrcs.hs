module IronclawOrcs where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


ironclawOrcs = (properties.name .~ "Ironclaw Orcs") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Orc]) . (properties.oracleText .~ "Ironclaw Orcs can't block creatures with power 2 or greater.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
