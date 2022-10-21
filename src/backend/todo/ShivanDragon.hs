module ShivanDragon where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


shivanDragon = (properties.name .~ "Shivan Dragon") . (properties.manaCost ?~ [GenSym 4,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Dragon]) . (properties.oracleText .~ "Flying (This creature can't be blocked except by creatures with flying or reach.)\n{R}: Shivan Dragon gets +1/+0 until end of turn.") . (properties.power .~ (Just (PT 5))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
