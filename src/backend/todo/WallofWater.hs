module WallofWater where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


wallofWater = (properties.name .~ "Wall of Water") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ [Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)\n{U}: Wall of Water gets +1/+0 until end of turn.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
