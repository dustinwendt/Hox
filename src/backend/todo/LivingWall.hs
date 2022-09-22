module LivingWall where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


livingWall = (properties.name .~ "Living Wall") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ [Defender]) . (properties.typeLine .~ TypeLine [] [Artifact,Creature] [CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)\n{1}: Regenerate Living Wall.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 6))) $ defaultCard
