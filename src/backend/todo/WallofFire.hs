module WallofFire where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


wallofFire = (properties.name .~ "Wall of Fire") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ [Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)\n{R}: Wall of Fire gets +1/+0 until end of turn.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
