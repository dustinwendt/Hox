module TwoHeadedGiantofForiys where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


twoHeadedGiantofForiys = (properties.name .~ "Two-Headed Giant of Foriys") . (properties.manaCost ?~ [GenSym 4,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ [Trample]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Giant]) . (properties.oracleText .~ "Trample
Two-Headed Giant of Foriys can block an additional creature each combat.") . (properties.power .~ (Just (PT 4))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
