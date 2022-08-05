module UthdenTroll where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


uthdenTroll = (properties.name .~ "Uthden Troll") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Troll]) . (properties.oracleText .~ "{R}: Regenerate Uthden Troll.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
