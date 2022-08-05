module SedgeTroll where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


sedgeTroll = (properties.name .~ "Sedge Troll") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Black,Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Troll]) . (properties.oracleText .~ "Sedge Troll gets +1/+1 as long as you control a Swamp.
{B}: Regenerate Sedge Troll.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
