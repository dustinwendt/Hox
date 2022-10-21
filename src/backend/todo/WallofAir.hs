module WallofAir where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


wallofAir = (properties.name .~ "Wall of Air") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ [Flying,Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wall]) . (properties.oracleText .~ "Defender, flying (This creature can't attack, and it can block creatures with flying.)") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
