module WallofIce where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


wallofIce = (properties.name .~ "Wall of Ice") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 7))) $ defaultCard
