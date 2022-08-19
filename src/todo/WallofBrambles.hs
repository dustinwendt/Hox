module WallofBrambles where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


wallofBrambles = (properties.name .~ "Wall of Brambles") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Plant,CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)\n{G}: Regenerate Wall of Brambles.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
