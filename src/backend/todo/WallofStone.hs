module WallofStone where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


wallofStone = (properties.name .~ "Wall of Stone") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ [Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 8))) $ defaultCard
