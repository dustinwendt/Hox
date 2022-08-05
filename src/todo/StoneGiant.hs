module StoneGiant where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


stoneGiant = (properties.name .~ "Stone Giant") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Giant]) . (properties.oracleText .~ "{T}: Target creature you control with toughness less than Stone Giant's power gains flying until end of turn. Destroy that creature at the beginning of the next end step.") . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
