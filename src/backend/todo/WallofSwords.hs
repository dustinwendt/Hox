module WallofSwords where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


wallofSwords = (properties.name .~ "Wall of Swords") . (properties.manaCost ?~ [GenSym 3,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ [Flying,Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)\nFlying") . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
