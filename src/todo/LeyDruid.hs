module LeyDruid where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


leyDruid = (properties.name .~ "Ley Druid") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Druid]) . (properties.oracleText .~ "{T}: Untap target land.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
