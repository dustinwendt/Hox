module WallofWood where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


wallofWood = (properties.name .~ "Wall of Wood") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
